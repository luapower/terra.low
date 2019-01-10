
--low: everyday Terra functions.
--Written by Cosmin Apreutesei. Public domain.

if not ... then require'low_test'; return; end

local glue = require'glue'
local ffi = require'ffi'

--usage: setfenv(1, require'low')
local low = {}; setmetatable(low, low).__index = _G; low.low = low

--usage: setfenv(1, low.C)
low.C = {}; setmetatable(low.C, low.C).__index = low

--TODO: remove these pending terra fix.
Strict.__newindex, Strict.__index = nil, nil

setfenv(1, low.C)

--ternary operator -----------------------------------------------------------

--NOTE: terralib.select() can also be used but it's not short-circuiting.
low.iif = macro(function(cond, t, f)
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

--min/max --------------------------------------------------------------------

low.min = macro(function(a, b) return `iif(a < b, a, b) end)
low.max = macro(function(a, b) return `iif(a > b, a, b) end)

--C include system -----------------------------------------------------------

low[ffi.os] = true --enable `if Windows then` in both Terra and Lua contexts.
local platos = {Windows = 'mingw', Linux = 'linux', OSX = 'osx'}
local lp_platform = platos[ffi.os]..'64'

low.paths = {L = '.', P = lp_platform}
local function P(s) return s:gsub('$(%a)', low.paths) end

package.path = package.path .. P'$L/bin/$P/lua/?.lua;$L/?.lua;$L/?/init.lua'
package.cpath = package.cpath .. P';$L/bin/mingw64/clib/?.dll'
package.terrapath = package.terrapath .. P'$L/?.t;$L/?/init.t'

low.include_loaders = {}

function low.include_loaders.freetype(header) --motivating example
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
	return glue.update(low.C, C)
end

function low.link(lib)
	terralib.linklibrary(P(lib))
end

function C:__call(cstring)
	return glue.update(self, terralib.includecstring(cstring))
end

--stdlib dependencies --------------------------------------------------------

include'stdio.h'
include'stdlib.h'
include'string.h'

--stdin/out/err --------------------------------------------------------------

local _stdin  = global(&_iobuf, nil)
local _stdout = global(&_iobuf, nil)
local _stderr = global(&_iobuf, nil)
local fdopen = Windows and _fdopen or fdopen

--exposed as macros so that they can be opened on demand on the first call.
stdin = macro(function()
	return quote
		if _stdin == nil then _stdin = fdopen(0, 'r') end
		in _stdin
	end
end)
stdout = macro(function()
	return quote
		if _stdout == nil then _stdout = fdopen(1, 'w') end
		in _stdout
	end
end)
stderr = macro(function()
	return quote
		if _stderr == nil then _stderr = fdopen(2, 'w') end
		in _stderr
	end
end)

--Lua-style print ------------------------------------------------------------

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
		elseif t == uint32   then add(fmt, '%uu\t'  ); add(args, arg)
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

--checked allocators ---------------------------------------------------------

low.allocs = function()
	local C = {}; setmetatable(C, C).__index = low.C
	local size_t = uint64

	terra C.malloc(size: size_t): &opaque
		var p = malloc(size)
		return p
	end
	terra C.calloc(n: size_t, size: size_t)
		var p = calloc(n, size)
		return p
	end
	terra C.realloc(p0: &opaque, size: size_t)
		var p = realloc(p0, size)
		return p
	end
	terra C.free(p: &opaque)
		free(p)
	end
	return C
end

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

--dynamic arrays -------------------------------------------------------------

low.dynarray_type = glue.memoize(function(T, size_t, resize_factor)
	size_t = size_t or uint32
	resize_factor = resize_factor or 2
	local arr = struct {
		size: size_t;
		len: size_t;
		data: &T;
	}
	function arr.metamethods.__cast(from, to, exp)
		if from == (`{}):gettype() then --initalize with empty tuple
			return `arr {0, 0, nil}
		end
	end
	terra arr:resize(size: size_t): bool
		var new_data = [&T](C.realloc(self.data, sizeof(T) * size))
		if new_data == nil then return false end
		self.data = new_data
		self.size = size
		self.len = min(size, self.len)
		return true
	end
	terra arr:free()
		self:resize(0)
	end
	terra arr:get(i: size_t)
		check(i >= 0 and i < self.len)
		return self.data[i]
	end
	terra arr:set(i: size_t, val: T): bool
		if i >= self.size then
			if not self:resize(max(i + 1, self.size * resize_factor)) then
				return false
			end
		end
		self.data[i] = val
		self.len = max(self.len, i + 1)
		return true
	end
	return arr
end)

low.dynarray = macro(function(T, size, size_t, resize_factor)
	local arr_t = dynarray_type(T:astype(), size_t, resize_factor)
	return quote var a: arr_t = {}; a:resize(size); in a end
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
