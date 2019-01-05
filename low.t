
--low: everyday Terra functions.
--Written by Cosmin Apreutesei. Public domain.

if not ... then require'low_test'; return; end

local glue = require'glue'
local ffi = require'ffi'

local low = setmetatable({}, {__index = _G}) --usage: setfenv(1, require'low')

--ternary operator -----------------------------------------------------------

low.iif = macro(function(cond, true_val, false_val)
	return quote
		var v: true_val:gettype()
		if cond then
			v = true_val
		else
			v = false_val
		end
	in
		v
	end
end)

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

low.C = setmetatable({}, {__index = low}) --usage: setfenv(1, low.C)

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

setfenv(1, low.C)
include'stdio.h'
include'stdlib.h'

--assert ---------------------------------------------------------------------

low.check = macro(function(e)
	local fdopen = ffi.abi'win' and _fdopen or fdopen
	return quote
		if not e then
			var stderr = fdopen(2, 'w')
			fprintf(stderr, ['assertion failed ' .. tostring(e.filename)
				.. ':' .. tostring(e.linenumber) .. ': ' .. tostring(e) .. '\n'])
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
		var lo = [ lo ]
		var hi = [ hi ]
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

--stacks ---------------------------------------------------------------------

low.stack = glue.memoize(function(T)
	local stack = struct {
		data: &T;
		size: int;
		len: int;
	}
	terra stack:alloc(size: int)
		self.data = [&T](malloc(sizeof(T) * size))
		self.size = size
		self.len = 0
	end
	terra stack:free()
		free(self.data)
		self.data = nil
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

--language utils -------------------------------------------------------------

return low
