
--low: everyday Terra functions.
--Written by Cosmin Apreutesei. Public domain.

if not ... then require'low_test'; return; end

local ffi = require'ffi'

local memoize = terralib.memoize
local function update(d,t) for k,v in pairs(t) do d[k]=v; end; return dt end

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

--min/max/clamp --------------------------------------------------------------

low.min = macro(function(a, b) return `iif(a < b, a, b) end)
low.max = macro(function(a, b) return `iif(a > b, a, b) end)
low.clamp = macro(function(x, m, M) return `min(max(x, m), M) end)
low.abs = macro(function(x) return `iif(x < 0, -x, x) end)

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

--overriding this built-in so that modules can depend on it being memoized.
local terralib_includec = terralib.includec
terralib.includec = memoize(function(header, ...)
	for _,loader in pairs(low.include_loaders) do
		local C = loader(header, ...)
		if C then return C end
	end
	return terralib_includec(header, ...)
end)

--terralib.includec variant that dumps symbols into low.C.
function low.include(header)
	return update(C, terralib.includec(header))
end

low.linklibrary = terralib.linklibrary

function C:__call(cstring)
	return update(self, terralib.includecstring(cstring))
end

function low.cached(C)
	local t = {}; setmetatable(t, t)
	t.__deps = {}
	function t:__index(k)
		if rawget(C, k) ~= nil then
			table.insert(t.__deps, k)
		end
		local v = C[k]
		self[k] = v
		return v
	end
	return t
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
		elseif t == bool     then add(fmt, '%s\t'   ); add(args, `iif(arg, 'true\t', 'false\t'));
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
				'assertion failed' .. ' '
				.. tostring(e.filename)
				.. ':' .. tostring(e.linenumber)
				.. ': ' .. (msg and msg:asvalue() or tostring(e)) .. '\n'
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
	in	p
	end
end)

--typed memset ---------------------------------------------------------------

low.fill = macro(function(rval, val, len)
	if len == nil then --fill(rval, len)
		val, len = nil, val
	end
	val = val or 0
	len = len or 1
	local size = sizeof(rval:gettype().type)
	return quote
		check(len >= 0)
		memset(rval, val, size * len)
	end
end)

--free lists -----------------------------------------------------------------

low.freelist = memoize(function(T)
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

low.growbuffer = memoize(function(T)
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

--add virtual fields to structs ----------------------------------------------

function low.addproperties(T)
	local p = {}
	T.metamethods.__entrymissing = macro(function(k, self)
		return `[p[k]]
	end)
	return p
end



return low
