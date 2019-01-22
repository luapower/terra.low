
--Lua+Terra standard library & flat vocabulary of tools.
--Written by Cosmin Apreutesei. Public domain.
--Intended to be used as global environment: setfenv(1, require'low').

if not ... then require'low_test'; return; end

local ffi = require'ffi'
local glue = require'glue'
local pp = require'pp'
local random = require'random'
local arr = require'dynarray'
local map = require'khash'

--The C namespace: include() and extern() dump symbols here.
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

--add virtual fields to structs ----------------------------------------------

function low.addproperties(T)
	local props = {}
	T.metamethods.__entrymissing = macro(function(k, self)
		return `[props[k]](self)
	end)
	return props
end

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
low.glue = glue
low.pp = pp
low.arr = arr
low.map = map

--promoting symbols to global ------------------------------------------------

--[[  Lua 5.1 std library use (promoted symbols not listed)

TODO:
	assert with format		glue.assert
	print with format			print(format(...))
	pcall with traceback		glue.pcall
	append -> add

Modules:
	table math io os string debug coroutine package

Used:
	type tostring tonumber
	setmetatable getmetatable rawget rawset rawequal
	next pairs ipairs
	print
	pcall xpcall error assert
	select unpack
	require load loadstring loadfile dofile
	setfenv getfenv
	s:rep s:sub s:upper s:lower
	s:find s:gsub s:gmatch s:match
	s:byte s:char
	io.stdin io.stdout io.stderr
	io.open io.popen io.lines io.tmpfile io.type
	os.execute os.rename os.remove
	os.getenv
	os.difftime os.date os.time
	arg _G
	collectgarbage newproxy
	math.log
	s:reverse s:dump s:format(=string.format)
	coroutine.status coroutine.running
	debug.getinfo
	package.path package.cpath package.config
	package.loaded package.searchpath package.loaders
	os.exit

Not used:
	table.maxn
	math.modf math.mod math.fmod math.log10 math.exp
	math.sinh math.cosh math.tanh

Never use:
	module gcinfo _VERSION
	math.huge(=1/0) math.pow math.ldexp
	s:len s:gfind os.clock
	table.getn table.foreach table.foreachi
	io.close io.input io.output io.read io.write io.flush
	os.tmpname os.setlocale
	package.loadlib package.preload package.seeall
	debug.*

]]

low.push   = table.insert
low.add    = table.insert
low.pop    = table.remove
low.concat = table.concat
low.sort   = table.sort
low.format = string.format
low.traceback = debug.traceback
low.yield    = coroutine.yield
low.resume   = coroutine.resume
low.cowrap   = coroutine.wrap
low.cocreate = coroutine.create

--[[  LuaJIT 2.1 std library use (promoted symbols not listed)

Modules:
	bit jit ffi

Used:
	ffi.new
	ffi.string ffi.cast ffi.sizeof ffi.istype ffi.typeof ffi.offsetof
	ffi.copy ffi.fill
	ffi.load ffi.cdef
	ffi.metatype ffi.gc
	ffi.errno
	ffi.C
	jit.off

Not used:
	bit.rol bit.ror bit.bswap bit.arshif bit.tobit bit.tohex
	ffi.alignof
	jit.flush

Never use:
	ffi.os ffi.abi ffi.arch
	jit.os jit.arch jit.version jit.version_num jit.on jit.status
	jit.attach jit.util jit.opt

]]

low.bnot = bit.bnot
low.shl = bit.lshift
low.shr = bit.rshift
low.band = bit.band
low.bor = bit.bor
low.xor = bit.bxor

--glue -----------------------------------------------------------------------

--[[  glue use (promoted symbols not listed)

Modules:
	glue.string

Used:
	glue.assert
	glue.keys
	glue.shift
	glue.addr glue.ptr
	glue.bin
	glue.collect
	glue.escape
	glue.floor
	glue.pcall glue.fcall glue.fpcall glue.protect
	glue.gsplit
	glue.inherit glue.object
	glue.malloc glue.free
	glue.printer
	glue.replacefile
	glue.fromhex glue.tohex
	glue.tuples
	glue.freelist glue.growbuffer
	glue.pack glue.unpack

Not used:
	glue.readpipe
	glue.reverse
	glue.cpath glue.luapath

]]

low.memoize = glue.memoize --same as terralib.memoize

low.update    = glue.update
low.merge     = glue.merge
low.attr      = glue.attr
low.count     = glue.count
low.index     = glue.index
low.sortedpairs = glue.sortedpairs

low.indexof   = glue.indexof
low.append    = glue.append
low.extend    = glue.extend

low.autoload  = glue.autoload

low.canopen   = glue.canopen
low.readfile  = glue.readfile
low.writefile = glue.writefile
low.lines     = glue.lines

low.starts = glue.starts
low.trim = glue.trim

--[[  Terra 1.0.0 std library use (promoted symbols not listed)

Used:
	terra quote escape struct
	global constant
	(u)int8|16|32|64 int long float double bool niltype opaque rawstring ptrdiff
	sizeof
	import

Not used:
	unit(=:isunit, ={})

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
	<numeric_type>.signed
	<numeric_type>:min()
	<numeric_type>:max()
	<pointer_type>.type

FFI objects:
	terralib.new
	terralib.cast
	terralib.typeof

Used rarely:
	terralib.load terralib.loadstring terralib.loadfile
	terralib.includec
	terralib.saveobj
	package.terrapath terralib.includepath

Not used yet:
	terralib.linkllvm terralib.linkllvmstring
	terralib.newlist
	terralib.version
	terralib.intrinsic
	terralib.select
	terralib.newtarget terralib.istarget
	terralib.terrahome
	terralib.systemincludes

Debugging:
	terralib.traceback
	terralib.backtrace
	terralib.disas
	terralib.lookupsymbol
	terralib.lookupline

Undocumented:
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
	terralib.jitcompilationunit
	terralib.target terralib.freetarget terralib.nativetarget terralib.inittarget
	terralib.newcompilationunit terralib.initcompilationunit terralib.freecompilationunit
	terralib.llvmsizeof

]]

low.char = int8
low.cstring = rawstring
low.codepoint = uint32
low.offsetof = terralib.offsetof

low.linklibrary = terralib.linklibrary
low.overload = terralib.overloadedfunction
low.newstruct = terralib.types.newstruct

--C include system -----------------------------------------------------------

local platos = {Windows = 'mingw', Linux = 'linux', OSX = 'osx'}
local lp_platform = platos[ffi.os]..'64'

low.path_vars = {L = '.', P = lp_platform}
local function P(s) return s:gsub('$(%a)', low.path_vars) end

--add luapower's standard paths relative to the current directory.
package.path = package.path .. P'$L/bin/$P/lua/?.lua;$L/?.lua;$L/?/init.lua'
package.cpath = package.cpath .. P';$L/bin/mingw64/clib/?.dll'
package.terrapath = package.terrapath .. P'$L/?.t;$L/?/init.t'

low.includec_loaders = {}

function low.includec_loaders.freetype(header) --motivating example
	local header = header:match'^freetype/(.*)'
	if header then
		return terralib.includecstring([[
			#include "ft2build.h"
			#include FT_]]..(header:upper():gsub('%.', '_')))
	end
end

function low.includepath(path)
	terralib.includepath = terralib.includepath .. P(';'..path)
end

--overriding this built-in so that modules can depend on it being memoized.
local terralib_includec = terralib.includec
terralib.includec = memoize(function(header, ...)
	for _,loader in pairs(low.includec_loaders) do
		local C = loader(header, ...)
		if C then return C end
	end
	return terralib_includec(header, ...)
end)

--terralib.includec variant that dumps symbols into low.C.
function low.include(header)
	return update(C, terralib.includec(header))
end

function low.extern(name, T)
	local func = terralib.externfunction(name, T)
	C[name] = func
	return func
end

function C:__call(cstring)
	return update(self, terralib.includecstring(cstring))
end

--stdlib dependencies --------------------------------------------------------

include'stdio.h'
include'stdlib.h'
include'string.h'
include'math.h'

--math module ----------------------------------------------------------------

low.PI    = math.pi
low.min   = macro(function(a, b) return `iif(a < b, a, b) end, math.min)
low.max   = macro(function(a, b) return `iif(a > b, a, b) end, math.max)
low.abs   = macro(function(x) return `iif(x < 0, -x, x) end, math.abs)
low.floor = macro(function(x) return `C.floor(x) end, math.floor)
low.ceil  = macro(function(x) return `C.ceil(x) end, math.ceil)
low.sqrt  = macro(function(x) return `C.sqrt(x) end, math.sqrt)
low.pow   = C.pow
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
low.inc = macro(function(lval, i) i=i or 1; return quote lval = lval + i end end)
low.dec = macro(function(lval, i) i=i or 1; return quote lval = lval - i end end)

--glue module ----------------------------------------------------------------

low.round = macro(function(x, p)
	if p and p ~= 1 then
		return `C.floor(x / p + .5) * p
	else
		return `C.floor(x)
	end
end, glue.round)
low.snap = low.round

low.clamp = macro(function(x, m, M)
	return `min(max(x, m), M)
end, glue.clamp)

low.lerp = macro(function(x, x0, x1, y0, y1)
	return `y0 + (x-x0) * ([double](y1-y0) / (x1 - x0))
end, glue.lerp)

low.pass = macro(function(...) return ... end, glue.pass)
low.noop = macro(function() return quote end end, glue.noop)

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
end, glue.binsearch)

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

--tostring -------------------------------------------------------------------

local function format_arg(arg, fmt, args, freelist)
	local t = arg:gettype()
		 if t == &int8    then add(fmt, '%s'   ); add(args, arg)
	elseif t == int8     then add(fmt, '%d'   ); add(args, arg)
	elseif t == uint8    then add(fmt, '%u'   ); add(args, arg)
	elseif t == int16    then add(fmt, '%d'   ); add(args, arg)
	elseif t == uint16   then add(fmt, '%u'   ); add(args, arg)
	elseif t == int32    then add(fmt, '%d'   ); add(args, arg)
	elseif t == uint32   then add(fmt, '%u'   ); add(args, arg)
	elseif t == int64    then add(fmt, '%lldL'); add(args, arg)
	elseif t == uint64   then add(fmt, '%lluU'); add(args, arg)
	elseif t == double   then add(fmt, '%.14g'); add(args, arg)
	elseif t == float    then add(fmt, '%.14g'); add(args, arg)
	elseif t == bool     then add(fmt, '%s'   ); add(args, `iif(arg, 'true', 'false'))
	elseif t:isarray() then
		add(fmt, '[')
		for i=0,t.N-1 do
			format_arg(`arg[i], fmt, args, freelist)
			if i < t.N-1 then add(fmt, ',') end
		end
		add(fmt, ']')
	elseif t:isstruct() then
		local __tostring = t.metamethods.__tostring
		if __tostring then
			__tostring(arg, format_arg, fmt, args, freelist)
		else
			add(fmt, tostring(t)..'{')
			local layout = t:getlayout()
			for i,e in ipairs(layout.entries) do
				add(fmt, e.key..'=')
				format_arg(`arg.[e.key], fmt, args, freelist)
				if i < #layout.entries then add(fmt, ',') end
			end
			add(fmt, '}')
		end
	elseif t:isfunction() then
		add(fmt, tostring(t)..'<%llx>'); add(args, arg)
	elseif t:ispointer() then
		add(fmt, tostring(t):gsub(' ', '')..'<%llx>'); add(args, arg)
	end
end

low.tostring = macro(function(arg, outbuf, maxlen)
	local fmt, args, freelist = {}, {}, {}
	format_arg(arg, fmt, args, freelist)
	fmt = concat(fmt)
	local snprintf = Windows and _snprintf or snprintf
	if outbuf then
		return quote
			snprintf(outbuf, maxlen, fmt, [args])
			[ freelist ]
		end
	else
		return quote
			var out = arr(char)
			if out:resize(32) then
				var n = snprintf(out.elements, out.size, fmt, [args])
				if n < 0 then
					out:free()
				elseif n < out.size then
					out.len = n+1
				else
					if not out:resize(n+1) then
						out:free()
					else
						assert(snprintf(out.elements, out.size, fmt, [args]) == n)
						out.len = n+1
					end
				end
			end
			[ freelist ]
			in out
		end
	end
end, tostring)

--Lua-style print ------------------------------------------------------------

low.prf = macro(function(...)
	local args = {...}
	return quote
		var stdout = stdout()
		fprintf(stdout, [args])
		fprintf(stdout, '\n')
		fflush(stdout)
	end
end)

low.print = macro(function(...)
	local fmt, args, freelist = {}, {}, {}
	local n = select('#', ...)
	for i=1,n do
		local arg = select(i, ...)
		format_arg(arg, fmt, args, freelist)
		add(fmt, i < n and '\t' or nil)
	end
	fmt = concat(fmt)
	return quote
		var stdout = stdout()
		fprintf(stdout, fmt, [args])
		fprintf(stdout, '\n')
		fflush(stdout)
		[ freelist ]
	end
end, print)

--assert ---------------------------------------------------------------------

low.assert = macro(function(expr, msg)
	return quote
		if not expr then
			var stderr = stderr()
			fprintf(stderr, [
				'assertion failed '
				.. (msg and '('..msg:asvalue()..') ' or '')
				.. tostring(expr.filename)
				.. ':' .. tostring(expr.linenumber)
				.. ': ' .. tostring(expr) .. '\n'
			])
			fflush(stderr)
			abort()
		end
	end
end, assert)

--clock ----------------------------------------------------------------------
--monotonic clock (can't go back or drift) in seconds with ~1us precision.

local clock
if Windows then
	extern('QueryPerformanceFrequency', {&int64}->int32)
	extern('QueryPerformanceCounter', {&int64}->int32)
	linklibrary'kernel32'
	local inv_qpf = global(double, 0)
	local terra init()
		var t: int64
		assert(QueryPerformanceFrequency(&t) ~= 0)
		inv_qpf = 1.0 / t --precision loss in e-10
	end
	clock = terra(): double
		if inv_qpf == 0 then init() end
		var t: int64
		assert(C.QueryPerformanceCounter(&t) ~= 0)
		return [double](t) * inv_qpf
	end
elseif Linux then
	--TODO: finish and test this
	include'time.h'
	linklibrary'rt'
	clock = terra(): double
		var t: timespec
		assert(clock_gettime(CLOCK_MONOTONIC, &tp) == 0)
		return t.tv_sec + t.tv_nsec / 1.0e9
	end
elseif OSX then
	--TODO: finish and test this
	clock = terra(): double
		return [double](mach_absolute_time())
	end
end
low.clock = macro(function() return `clock() end, terralib.currenttimeinseconds)

--typed calloc ---------------------------------------------------------------

local calloc = macro(function(size) return `calloc(size, 1) end)
low.new = macro(function(T, len, init)
	len = len or 1
	T = T:astype()
	local alloc = (init == nil or init == true) and calloc or malloc
	return quote
		var len: uint64 = [len]
		assert(len >= 0)
		var p: &T
		if len == 0 then p = nil else p = [&T](alloc(len * sizeof(T))) end
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

--typed memmove --------------------------------------------------------------

low.copy = macro(function(dst, src, len)
	local T = dst:gettype().type
	assert(T == src:gettype().type)
	return quote memmove(dst, src, len * sizeof(T)) in dst end
end)

--default hash function ------------------------------------------------------

low.hash = macro(function(size_t, k, len, seed) --FNV-1A hash
	size_t = size_t:astype()
	seed = seed or 0x811C9DC5
	return quote
		var d: size_t = seed
		var k = [&int8](k)
		for i = 0, len do
			d = (d ^ k[i]) * 16777619
		end
		in d
	end
end)
low.hash32 = macro(function(k, len, seed) return `hash(int32, k, len, seed) end)
low.hash64 = macro(function(k, len, seed) return `hash(int64, k, len, seed) end)

--variable-length struct -----------------------------------------------------

low.VLS = macro(function(T, VT, len)
	T = T:astype()
	VT = VT:astype()
	return quote
		assert(len >= 0)
		var v = [&T](malloc(sizeof(T) + sizeof(VT) + len))
		memset(v, 0, sizeof(T))
		in v
	end
end)

--checked allocators ---------------------------------------------------------

low.allocs = function()
	local C = {}; setmetatable(C, C).__index = low.C
	local size_t = uint64

	terra C.realloc(p0: &opaque, size: size_t)
		var p = realloc(p0, size)
		--TODO: track memory here
		return p
	end
	--the following functions are based on realloc only.
	terra C.malloc(size: size_t): &opaque
		return C.realloc(nil, size)
	end
	terra C.calloc(n: size_t, size: size_t)
		var p = C.realloc(nil, n * size)
		if p ~= nil then memset(p, 0, n * size) end
		return p
	end
	terra C.free(p: &opaque)
		C.realloc(p, 0)
	end
	return C
end

return low
