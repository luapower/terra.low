
--low: everyday Terra functions.
--Written by Cosmin Apreutesei. Public domain.

if not ... then require'low_test'; return; end

local ffi = require'ffi'
local glue = require'glue'
local random = require'random'

local memoize = glue.memoize
local update = glue.update
local round = glue.round
local clamp = glue.clamp
local lerp = glue.lerp
local binsearch = glue.binsearch

--usage: setfenv(1, require'low')
local C = {}; setmetatable(C, C); C.__index = _G
local low = {}; setmetatable(low, low); low.__index = C

--TODO: remove these pending terra fix.
Strict.__newindex, Strict.__index = nil, nil

setfenv(1, low)

--ternary operator -----------------------------------------------------------

--NOTE: terralib.select() can also be used but it's not short-circuiting.
low.iif = macro(function(cond, t, f)
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

--OS defines -----------------------------------------------------------------

low.Windows = false
low.Linux = false
low.OSX = false
low.BSD = false
low.POSIX = false
low[ffi.os] = true

--exposing submodules --------------------------------------------------------

low.ffi = ffi
low.low = low
low.C = C

--promoting to global --------------------------------------------------------

low.linklibrary = terralib.linklibrary
low.overloadedfunction = terralib.overloadedfunction

function low.externfunction(name, T)
	local func = terralib.externfunction(name, T)
	C[name] = func
	return func
end

--[[

Rreserved words:
	terra escape quote import

Modules:
	terralib

Types:
	float double int8 uint8 int16 uint16 int32 uint32 int64 uint64
	bool niltype opaque
	int(=int32) uint=(uint32) long(=int64) intptr(=uint64) rawstring(=&int8)

Constructors:
	unpacktuple unpackstruct
	vector vectorof global constant macro tuple symbol label

Only from Terra:
	array arrayof

Type checks:
	terralib.type
	terralib.isfunction
	terralib.isoverloadedfunction
	terralib.isintegral
	terralib.types.istype
	terralib.islabel
	terralib.isquote
	terralib.issymbol
	terralib.isconstant
	terralib.isglobalvar
	terralib.ismacro
	terralib.isfunction
	terralib.islist
	terralib.israwlist
	sizeof
	<numeric_type>.signed
	<pointer_type>.type

FFI objects:
	terralib.new
	terralib.cast
	terralib.typeof
	terralib.offsetof

Used rarely:
	terralib.load terralib.loadstring
	terralib.includecstring terralib.includec
	terralib.saveobj
	package.terrapath terralib.includepath
	terralib.linklibrary

Not used yet:
	terralib.newlist
	terralib.loadfile
	terralib.version
	terralib.intrinsic
	terralib.select
	terralib.newtarget terralib.istarget
	terralib.terrahome
	terralib.types.newstruct
	terralib.systemincludes

Never use:
	terralib.memoize(=glue.memoize)
	terralib.newlabel(=label)
	terralib.newsymbol(=symbol)
	terralib.createmacro(=macro)

Debugging:
	terralib.traceback
	terralib.backtrace
	terralib.disas
	terralib.lookupsymbol
	terralib.lookupline

Undocumented / debugging:
	terralib.SymbolInfo
	terralib.LineInfo
	terralib.fulltrace
	terralib.debuginfo
	terralib.diagnostics terralib.newdiagnostics
	terralib.gcdebug
	terralib.initdebugfns
	terralib.isdebug
Undocumented / CUDA:
	terralib.cudahome
	terralib.cudalibpaths
	terralib.cudaloaderror
	terralib.cudatarget
Undocumented / language extensions:
	terralib.languageextension
	terralib.importlanguage
	terralib.unimportlanguages
	terralib.runlanguage
Undocumented / ???:
	terralib.dumpmodule
	terralib.types
	terralib.types.funcpointer
	terralib.asm
	operator
	terralib.pointertolightuserdata
	terralib.registerinternalizedfiles
	terralib.bindtoluaapi
	terralib.internalmacro
	terralib.printraw
	terralib.kinds
	terralib.definequote terralib.newquote
	terralib.anonfunction
	terralib.attrstore
	terralib.irtypes
	terralib.registercfile
	terralib.compilationunitaddvalue
	terralib.systemincludes
	terralib.jit
	terralib.anonstruct
	terralib.disassemble
	terralib.environment
	terralib.makeenv
	terralib.newenvironment
	terralib.getvclinker
	terralib.istree
	terralib.attrload
	terralib.defineobjects
	terralib.newanchor
	terralib.initcompilationunit
	terralib.jitcompilationunit
	terralib.target terralib.freetarget terralib.nativetarget terralib.inittarget
	terralib.newcompilationunit terralib.freecompilationunit
	terralib.vshome
	terralib.isverbose
	terralib.llvm_gcdebugmetatable
	terralib.linkllvm
	terralib.linkllvmstring
	terralib.llvmversion
	terralib.llvmsizeof
	terralib.linkllvmimpl
	terralib.linklibraryimpl
	terralib.saveobjimpl
	terralib._trees
	terralib.__terrastate
	Strict

]]

--C include system -----------------------------------------------------------

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
include'math.h'

--integer limits -------------------------------------------------------------

local maxint = {}
local minint = {}
maxint[int8  ] = `[int8  ]( 0x7F)
minint[int8  ] = `[int8  ](-0x80)
maxint[int16 ] = `[int16 ]( 0x7FFF)
minint[int16 ] = `[int16 ](-0x8000)
maxint[int32 ] = `[int32 ]( 0x7FFFFFFF)
minint[int32 ] = `[int32 ](-0x80000000)
maxint[int64 ] = ` 0x7FFFFFFFFFFFFFFFLL
minint[int64 ] = `-0x8000000000000000LL
maxint[uint8 ] = `[uint8 ](0xFF)
maxint[uint16] = `[uint16](0xFFFF)
maxint[uint32] = `[uint32](0xFFFFFFFF)
maxint[uint64] = ` 0xFFFFFFFFFFFFFFFFULL
low.maxint = macro(function(T) return maxint[T:astype()] end)
low.minint = macro(function(T) return minint[T:astype()] end)

--math module ----------------------------------------------------------------

low.PI    = math.pi
low.min   = macro(function(a, b) return `iif(a < b, a, b) end, math.min)
low.max   = macro(function(a, b) return `iif(a > b, a, b) end, math.max)
low.abs   = macro(function(x) return `iif(x < 0, -x, x) end, math.abs)
low.floor = macro(function(x) return `C.floor(x) end, math.floor)
low.ceil  = macro(function(x) return `C.ceil(x) end, math.ceil)
low.sqrt  = macro(function(x) return `C.sqrt(x) end, math.sqrt)
low.sin   = macro(function(x) return `C.sin(x) end, math.sin)
low.cos   = macro(function(x) return `C.cos(x) end, math.cos)
low.tan   = macro(function(x) return `C.tan(x) end, math.tan)
low.asin  = macro(function(x) return `C.asin(x) end, math.sin)
low.acos  = macro(function(x) return `C.acos(x) end, math.sin)
low.atan  = macro(function(x) return `C.atan(x) end, math.sin)
low.atan2 = macro(function(y, x) return `C.atan2(y, x) end, math.sin)
low.deg   = macro(function(r) return `r * (180.0 / PI) end, math.deg)
low.rad   = macro(function(d) return `d * (PI / 180.0) end, math.rad)
low.random    = random.random
low.randomize = random.randomize

--glue/math module -----------------------------------------------------------

low.round = macro(function(x, p)
	if p and p ~= 1 then
		return `C.floor(x / p + .5) * p
	else
		return `C.floor(x)
	end
end, round)
low.snap = low.round

low.clamp = macro(function(x, m, M)
	return `min(max(x, m), M)
end, clamp)

low.lerp = macro(function(x, x0, x1, y0, y1)
	return `y0 + (x-x0) * ([double](y1-y0) / (x1 - x0))
end, lerp)

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
	in i
	end
end, binsearch)

--stdin/out/err --------------------------------------------------------------

local _stdin  = global(&_iobuf, nil)
local _stdout = global(&_iobuf, nil)
local _stderr = global(&_iobuf, nil)
local fdopen = Windows and _fdopen or fdopen

--exposed as macros so that they can be opened on demand on the first call.
low.stdin = macro(function()
	return quote
		if _stdin == nil then _stdin = fdopen(0, 'r') end in _stdin
	end
end)
low.stdout = macro(function()
	return quote
		if _stdout == nil then _stdout = fdopen(1, 'w') end in _stdout
	end
end)
low.stderr = macro(function()
	return quote
		if _stderr == nil then _stderr = fdopen(2, 'w') end in _stderr
	end
end)

--Lua-style print ------------------------------------------------------------

low.prf = macro(function(...)
	local args = {...}
	return quote
		var stdout = stdout()
		fprintf(stdout, [args])
		fflush(stdout)
	end
end)

low.print = macro(function(...)
	local args = {}
	local fmt = {}
	local add = table.insert
	local function format(arg)
		local t = arg:gettype()
		    if t == &int8    then add(fmt, "%s\t"   ); add(args, arg)
		elseif t == int8     then add(fmt, '%d\t'   ); add(args, arg)
		elseif t == uint8    then add(fmt, '%u\t'   ); add(args, arg)
		elseif t == int16    then add(fmt, '%d\t'   ); add(args, arg)
		elseif t == uint16   then add(fmt, '%u\t'   ); add(args, arg)
		elseif t == int32    then add(fmt, '%d\t'   ); add(args, arg)
		elseif t == uint32   then add(fmt, '%u\t'   ); add(args, arg)
		elseif t == int64    then add(fmt, '%lldL\t'); add(args, arg)
		elseif t == uint64   then add(fmt, '%lluU\t'); add(args, arg)
		elseif t == double   then add(fmt, '%.14g\t'); add(args, arg)
		elseif t == float    then add(fmt, '%.14g\t'); add(args, arg)
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
end, print)

--assert ---------------------------------------------------------------------

low.assert = macro(function(e, msg)
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
end, assert)

--clock ----------------------------------------------------------------------

local clock
if Windows then
	externfunction('QueryPerformanceFrequency', {&int64}->int32)
	externfunction('QueryPerformanceCounter', {&int64}->int32)
	linklibrary'kernel32'
	local inv_qpf = global(double, 0)
	local terra init()
		var t: int64
		assert(QueryPerformanceFrequency(&t) ~= 0)
		inv_qpf = 1.0 / t --precission loss in e-10
	end
	clock = terra(): double
		if inv_qpf == 0 then init() end
		var t: int64
		assert(C.QueryPerformanceCounter(&t) ~= 0)
		return [double](t) * inv_qpf
	end
elseif Linux then
	linklibrary'rt'
elseif OSX then
	--
end
low.clock = macro(function() return `clock() end, terralib.currenttimeinseconds)

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

--typed calloc ---------------------------------------------------------------

low.new = macro(function(T, len)
	len = len or 1
	T = T:astype()
	return quote
		var len: int = [len]
		assert(len >= 0)
		var p: &T = [&T](calloc(len, sizeof(T)))
	in	p
	end
end)

--typed memset ---------------------------------------------------------------

low.fill = macro(function(lval, val, len)
	if len == nil then --fill(lval, len)
		val, len = nil, val
	end
	val = val or 0
	len = len or 1
	local size = sizeof(lval:gettype().type)
	return quote
		assert(len >= 0)
		memset(lval, val, size * len)
		in lval
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
	local props = {}
	T.metamethods.__entrymissing = macro(function(k, self)
		return `[props[k]](self)
	end)
	return props
end



return low
