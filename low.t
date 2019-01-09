
--low: everyday Terra functions.
--Written by Cosmin Apreutesei. Public domain.

if not ... then require'low_test'; return; end

local glue = require'glue'
local ffi = require'ffi'

local low = setmetatable({}, {__index = _G}) --usage: setfenv(1, require'low')
low.low = low
low.C = setmetatable({}, {__index = low}) --usage: setfenv(1, low.C)
Strict.__newindex, Strict.__index = nil, nil --TODO: remove these pending terra fix.
setfenv(1, low.C)

--ternary operator -----------------------------------------------------------

--NOTE: terralib.select() can also be used but it's not short-circuiting.
low.iif = macro(function(cond, t, f)
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

--min/max --------------------------------------------------------------------

low.min = macro(function(a, b) return `iif(a < b, a, b) end)
low.max = macro(function(a, b) return `iif(a > b, a, b) end)

--compiler -------------------------------------------------------------------

local platos = {Windows = 'mingw', Linux = 'linux', OSX = 'osx'}
low.platform = platos[ffi.os]..(ffi.abi'32bit' and '32' or '64')

low.paths = {L = '.', P = low.platform}
local function P(s) return s:gsub('$(%a)', low.paths) end

package.path = package.path .. P'$L/bin/$P/lua/?.lua;$L/?.lua;$L/?/init.lua'
package.cpath = package.cpath .. P';$L/bin/mingw64/clib/?.dll'
package.terrapath = package.terrapath .. P'$L/?.t;$L/?/init.t'

low.include_loaders = {}

function low.include_loaders.freetype(header)
	local header = header:match'^freetype/(.*)'
	if header then
		return terralib.includecstring([[
			#include "ft2build.h"
			#include FT_]]..(header:upper():gsub('%.', '_')))
	end
end

function low.I(path)
	terralib.includepath = terralib.includepath .. P(';'..path)
end

local includec = glue.memoize(function(header)
	return terralib.includec(header)
end)
function low.include(header)
	local C
	for _,loader in pairs(low.include_loaders) do
		C = loader(header)
		if C then break end
	end
	C = C or includec(header)
	glue.update(low.C, C)
end

function low.link(lib)
	terralib.linklibrary(P(lib))
end

--includes -------------------------------------------------------------------

include'stdio.h'
include'stdlib.h'
include'string.h'

--print ----------------------------------------------------------------------

local _stdout = global(&_iobuf, nil)
local _stderr = global(&_iobuf, nil)

stdout = macro(function()
	return quote
		if _stdout == nil then _stdout = _fdopen(1, 'w') end
		in _stdout
	end
end)
stderr = macro(function()
	return quote
		if _stderr == nil then _stderr = _fdopen(2, 'w') end
		in _stderr
	end
end)

prf = macro(function(...)
	local args = {...}
	return quote
		var stdout = stdout()
		fprintf(stdout, [args])
		fflush(stdout)
	end
end)

pr = macro(function(...)
	local args = {}
	local fmt = {}
	local add = table.insert
	local function format(arg)
		local t = arg:gettype()
		    if t == &int8    then add(fmt, '%s\t'   ); add(args, arg)
		elseif t == int8     then add(fmt, '%dc\t'  ); add(args, arg)
		elseif t == uint8    then add(fmt, '%ub\t'  ); add(args, arg)
		elseif t == int16    then add(fmt, '%ds\t'  ); add(args, arg)
		elseif t == uint16   then add(fmt, '%uw\t'  ); add(args, arg)
		elseif t == int32    then add(fmt, '%d\t'   ); add(args, arg)
		elseif t == uint32   then add(fmt, '%u\t'   ); add(args, arg)
		elseif t == int64    then add(fmt, '%lldL\t'); add(args, arg)
		elseif t == uint64   then add(fmt, '%lluU\t'); add(args, arg)
		elseif t == double   then add(fmt, '%gd\t'  ); add(args, arg)
		elseif t == float    then add(fmt, '%gf\t'  ); add(args, arg)
		elseif t:isarray() then
			add(fmt, '[')
			local j=#fmt
			for i=0,t.N-1 do
				format(`arg[i])
			end
			for i=j,#fmt do fmt[i]=fmt[i]:gsub('\t', i<#fmt and ',' or '') end
			add(fmt, ']\t')
		elseif t:isstruct() then
			add(fmt, t.name:gsub('anon','')..'{')
			local layout = t:getlayout()
			local j=#fmt
			for i,e in ipairs(layout.entries) do
				add(fmt, e.key..'=')
				format(`arg.[e.key])
			end
			for i=j,#fmt do fmt[i]=fmt[i]:gsub('\t', i<#fmt and ',' or '') end
			add(fmt, '}\t')
		elseif t:isfunction() then
			add(fmt, tostring(t)..'<%llx>\t'); add(args, arg)
		elseif t:ispointer() then
			add(fmt, tostring(t):gsub(' ', '')..'<%llx>\t'); add(args, arg)
		end
	end
	for i=1,select('#', ...) do
		local arg = select(i, ...)
		format(arg)
	end
	fmt = table.concat(fmt):gsub('\t$', '')
	return quote
		var stdout = stdout()
		fprintf(stdout, fmt, [args])
		fprintf(stdout, '\n')
		fflush(stdout)
	end
end)

--assert ---------------------------------------------------------------------

low.check = macro(function(e, msg)
	local fdopen = ffi.abi'win' and _fdopen or fdopen
	return quote
		if not e then
			var stderr = stderr()
			fprintf(stderr, [
				(msg or 'assertion failed') .. ' '
				.. tostring(e.filename)
				.. ':' .. tostring(e.linenumber)
				.. ': ' .. tostring(e) .. '\n'
			])
			fflush(stderr)
			abort()
		end
	end
end)

--binsearch macro ------------------------------------------------------------

--binary search for an insert position that keeps the array sorted.
local less = macro(function(t, i, v) return `t[i] <  v end)
low.binsearch = macro(function(v, t, lo, hi, cmp)
	cmp = cmp or less
	return quote
		var lo = [lo]
		var hi = [hi]
		var i = hi + 1
		while true do
			if lo < hi then
				var mid: int = lo + (hi - lo) / 2
				if cmp(t, mid, v) then
					lo = mid + 1
				else
					hi = mid
				end
			else
				if lo == hi and not cmp(t, lo, v) then
					i = lo
				end
				break
			end
		end
	in
		i
	end
end)

--typed calloc ---------------------------------------------------------------

low.new = macro(function(T, len)
	len = len or 1
	T = T:astype()
	return quote
		var len: int = [len]
		check(len >= 0)
		var p: &T = [&T](calloc(len, sizeof(T)))
	in
		p
	end
end)

--typed memset ---------------------------------------------------------------

low.fill = macro(function(rval, val, len)
	val = val or 0
	len = len or 1
	local size = sizeof(rval:gettype().type)
	return quote
		check(len >= 0)
		memset(rval, val, size * len)
	end
end)

--stacks ---------------------------------------------------------------------

low.stack = glue.memoize(function(T)
	local stack = struct {
		data: &T;
		size: int;
		len: int;
	}
	terra stack:alloc(size: int)
		self.data = new(T, size)
		self.size = size
		self.len = 0
	end
	terra stack:free()
		free(self.data)
		self.data = nil
		self.size = 0
		self.len = 0
	end
	terra stack:push(elem: T)
		check(self.len < self.size)
		self.data[self.len] = elem
		self.len = self.len + 1
	end
	terra stack:pop(): T
		check(self.len >= 1)
		self.len = self.len - 1
		return self.data[self.len]
	end
	return stack
end)

--free lists -----------------------------------------------------------------

low.freelist = glue.memoize(function(T)
	local freelist = struct {
		data: &&T;
		size: int;
		len: int;
	}
	terra freelist:alloc(size: int)
		self.data = new([&T], size)
		self.size = size
		self.len = 0
	end
	terra freelist:free()
		for i=0,self.len do
			free(self.data[i])
		end
		free(self.data)
		fill(self)
	end
	terra freelist:new()
		if self.len > 0 then
			self.len = self.len - 1
			fill(self.data[self.len])
			return self.data[self.len]
		else
			return new(T)
		end
	end
	terra freelist:release(p: &T)
		if self.len < self.size then
			self.data[self.len] = p
			self.len = self.len + 1
		else
			free(p)
		end
	end
	return freelist
end)

--ever-growing buffer --------------------------------------------------------

low.growbuffer = glue.memoize(function(T)
	local growbuffer = struct {
		data: &T;
		size: int;
	}
	terra growbuffer:alloc()
		fill(self)
	end
	terra growbuffer:free()
		free(self.data)
		fill(self)
	end
	terra growbuffer.metamethods.__apply(self: &growbuffer, size: int)
		if self.size < size then
			self.data = new(T, size)
			self.size = size
		end
		return self.data
	end
	return growbuffer
end)

--language utils -------------------------------------------------------------

return low
