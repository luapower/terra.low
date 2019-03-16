
--Lua+Terra standard library & flat vocabulary of tools.
--Written by Cosmin Apreutesei. Public domain.

--Intended to be used as global environment: setfenv(1, require'low').

if not ... then require'low_test'; return; end

--dependencies ---------------------------------------------------------------

--The C namespace: include() and extern() dump symbols here.
local C = {}; setmetatable(C, C); C.__index = _G
local low = {}; setmetatable(low, low); low.__index = C

setfenv(1, low)

low.low  = low
low.C    = C
low.ffi  = require'ffi'
low.zone = require'jit.zone'
low.glue = require'glue'
low.pp   = require'pp'

glue.autoload(low, {
	arrview   = function() low.arrview = require'arrayview' end,
	arr       = function() low.arr = require'dynarray' end,
	map       = function() low.map = require'khash' end,
	random    = function() low.random = require'random'.random end,
	randomize = function() low.randomize = require'random'.randomize end,
})

--promoting symbols to global ------------------------------------------------

--[[  Lua 5.1 std library use (promoted symbols not listed)

TODO:
	pcall with traceback		glue.pcall

Modules:
	table math io os string debug coroutine package

Used:
	type tostring tonumber
	setmetatable getmetatable rawget rawset rawequal
	next pairs ipairs
	print
	pcall xpcall error assert
	select
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
low.pop    = table.remove
low.add    = table.insert
low.insert = table.insert
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

low.Windows = false
low.Linux = false
low.OSX = false
low.BSD = false
low.POSIX = false
low[ffi.os] = true

--[[  glue use (promoted symbols not listed)

Modules:
	glue.string

Used:
	glue.map
	glue.keys
	glue.shift
	glue.addr glue.ptr
	glue.bin
	glue.collect
	glue.esc
	glue.floor glue.ceil
	glue.pcall glue.fcall glue.fpcall glue.protect
	glue.gsplit
	glue.inherit glue.object
	glue.malloc glue.free
	glue.printer
	glue.replacefile
	glue.fromhex glue.tohex
	glue.freelist glue.growbuffer

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
low.writefile = glue.writefile
low.lines     = glue.lines

low.pack   = glue.pack
low.unpack = glue.unpack

string.starts = glue.starts
string.trim   = glue.trim

--[[  Terra 1.0.0 std library use (promoted symbols not listed)

Used:
	terra macro quote escape struct var global constant tuple arrayof
	(u)int8|16|32|64 int long float double bool niltype opaque rawstring ptrdiff
	sizeof unpacktuple unpackstruct
	import

Not used:
	unit(=:isunit, ={})

Type checks:
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

low.type = terralib.type

low.char = int8
low.enum = int8
low.num = double --Lua-compat type
low.codepoint = uint32
low.offsetof = terralib.offsetof

pr = terralib.printraw
low.linklibrary = terralib.linklibrary
low.overload = terralib.overloadedfunction
low.newstruct = terralib.types.newstruct

function low.istuple(T)
	return type(T) == 'terratype' and T.convertible == 'tuple'
end

--ternary operator -----------------------------------------------------------

--NOTE: terralib.select() can also be used but it's not short-circuiting.
low.iif = macro(function(cond, t, f)
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

--struct packing constructor -------------------------------------------------

local function entry_size(e)
	if e.type then
		return sizeof(e.type)
	else --union
		local size = 0
		for _,e in ipairs(e) do
			size = max(size, entry_size(e))
		end
		return size
	end
end
function low.packstruct(T)
	sort(T.entries, function(e1, e2)
		return entry_size(e1) > entry_size(e2)
	end)
end

--extensible struct metamethods ----------------------------------------------

local function override(mm, T, f, ismacro)
	local f0 = T.metamethods[mm]
	local f0 = f0 and ismacro and f0.fromterra or f0 or noop
	--TODO: see why errors are lost in recursive calls to __getmethod
	--and remove this whole hack with pcall and pass.
	local function pass(ok, ...)
		if not ok then
			print(...)
			os.exit(-1)
		end
		return ...
	end
	local f = function(...)
		return pass(pcall(f, f0, ...))
	end
	if ismacro then f = macro(f) end
	T.metamethods[mm] = f
	return T
end

local function before(mm, T, f, ...)
	local f = function(inherited, ...)
		return f(...) or inherited(...)
	end
	return override(mm, T, f, ...)
end
local function after(mm, T, f, ...)
	local f = function(inherited, ...)
		return inherited(...) or f(...)
	end
	return override(mm, T, f, ...)
end

function low.entrymissing  (T, f) return override('__entrymissing', T, f, true) end
function low.methodmissing (T, f) return override('__methodmissing', T, f, true) end
function low.setentry      (T, f) return override('__setentry', T, f, true) end
function low.getentries    (T, f) return override('__getentries', T, f) end
function low.getmethod     (T, f) return override('__getmethod', T, memoize(f)) end

function low.before_entrymissing  (T, f) return before('__entrymissing', T, f, true) end
function low.before_methodmissing (T, f) return before('__methodmissing', T, f, true) end
function low.before_setentry      (T, f) return before('__setentry', T, f, true) end
function low.before_getentries    (T, f) return before('__getentries', T, f) end
function low.before_getmethod     (T, f) return before('__getmethod', T, memoize(f)) end

function low.after_entrymissing  (T, f) return after('__entrymissing', T, f, true) end
function low.after_methodmissing (T, f) return after('__methodmissing', T, f, true) end
function low.after_setentry      (T, f) return after('__setentry', T, f, true) end
function low.after_getentries    (T, f) return after('__getentries', T, f) end
function low.after_getmethod     (T, f) return after('__getmethod', T, memoize(f)) end

--activate macro-based assignable properties in structs.
function low.addproperties(T, props)
	props = props or {}
	T.properties = props
	return after_entrymissing(T, function(k, self)
		local prop = props[k]
		if type(prop) == 'terramacro' or type(prop) == 'terrafunction' then
			return `prop(self)
		else
			return prop --quote or Lua constant value
		end
	end)
end

--forward t.name to t.sub.name (for anonymous structs and such).
function low.forwardproperties(sub)
	return function(T)
		return after_entrymissing(T, function(k, self)
			return `self.[sub].[k]
		end)
	end
end

--activate getters and setters in structs.
function low.gettersandsetters(T)
	T.gettersandsetters = true
	after_entrymissing(T, function(name, obj)
		if T.addmethods then T.addmethods() end
		if T.methods['get_'..name] then
			return `obj:['get_'..name]()
		end
	end)
	after_setentry(T, function(name, obj, rhs)
		if T.addmethods then T.addmethods() end
		if T.methods['set_'..name] then
			return quote obj:['set_'..name](rhs) end
		end
	end)
	return T
end

--lazy method publishing pattern for containers
--workaround for terra issue #348.
--NOTE: __methodmissing is no longer called if __getmethod is present!
function low.addmethods(T, addmethods_func)
	T.addmethods = function(self, name)
		T.addmethods = nil
		addmethods_func()
	end
	return after_getmethod(T, function(self, name)
		if T.addmethods then T.addmethods() end
		return self.methods[name]
	end)
end

--wrapping opaque structs declared in C headers
--workaround for terra issue #351.
function low.wrapopaque(T)
	return getentries(T, function() return {} end)
end

--C include system -----------------------------------------------------------

local platos = {Windows = 'mingw', Linux = 'linux', OSX = 'osx'}
low.platform = platos[ffi.os]..'64'

low.path_vars = {L = '.', P = platform}
local function P(s) return s:gsub('$(%a)', low.path_vars) end

--add luapower's standard paths relative to the current directory.
package.path = package.path .. P'$L/bin/$P/lua/?.lua;$L/?.lua;$L/?/init.lua'
package.cpath = package.cpath .. P';$L/bin/mingw64/clib/?.dll'
package.terrapath = package.terrapath .. P'$L/?.t;$L/?/init.t'

low.includec_loaders = {} --{name -> loader(header_name)}

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
	zone'include'
	update(C, terralib.includec(header))
	zone()
	return C
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

--TODO: manually type only what we use from these.
include'stdio.h'
include'stdlib.h'
include'string.h'
include'math.h'

--math module ----------------------------------------------------------------

low.PI     = math.pi
low.min    = macro(function(a, b) return `iif(a < b, a, b) end, math.min)
low.max    = macro(function(a, b) return `iif(a > b, a, b) end, math.max)
low.abs    = macro(function(x) return `iif(x < 0, -x, x) end, math.abs)
low.floor  = macro(function(x) return `C.floor(x) end, math.floor)
low.ceil   = macro(function(x) return `C.ceil(x) end, math.ceil)
low.sqrt   = macro(function(x) return `C.sqrt(x) end, math.sqrt)
low.pow    = C.pow
low.log    = macro(function(x) return `C.log(x) end, math.log)
low.sin    = macro(function(x) return `C.sin(x) end, math.sin)
low.cos    = macro(function(x) return `C.cos(x) end, math.cos)
low.tan    = macro(function(x) return `C.tan(x) end, math.tan)
low.asin   = macro(function(x) return `C.asin(x) end, math.sin)
low.acos   = macro(function(x) return `C.acos(x) end, math.sin)
low.atan   = macro(function(x) return `C.atan(x) end, math.sin)
low.atan2  = macro(function(y, x) return `C.atan2(y, x) end, math.sin)
low.deg    = macro(function(r) return `r * (180.0 / PI) end, math.deg)
low.rad    = macro(function(d) return `d * (PI / 180.0) end, math.rad)
--go full Pascal :)
low.inc    = macro(function(lval, i) i=i or 1; return quote lval = lval + i in lval end end)
low.dec    = macro(function(lval, i) i=i or 1; return quote lval = lval - i in lval end end)
low.swap   = macro(function(a, b) return quote var c = a; a = b; b = c end end)
low.isodd  = macro(function(x) return `x % 2 == 1 end)
low.iseven = macro(function(x) return `x % 2 == 0 end)
low.isnan  = macro(function(x) return `x ~= x end)
low.inf    = 1/0
low.nan    = 0/0
low.maxint = int:max()
low.minint = int:min()

--round up an integer to the next integer that is a power of 2.
low.nextpow2 = macro(function(x)
	local T = x:gettype()
	if T:isintegral() then
		local bytes = sizeof(T)
		return quote
			var x = x
			x = x - 1
			x = x or (x >>  1)
			x = x or (x >>  2)
			x = x or (x >>  4)
			if bytes >= 2 then x = x or (x >>  8) end
			if bytes >= 4 then x = x or (x >> 16) end
			if bytes >= 8 then x = x or (x >> 32) end
			x = x + 1
			in x
		end
	end
	error('unsupported type ', x:gettype())
end)

--math from glue -------------------------------------------------------------

low.round = macro(function(x, p)
	if p and p ~= 1 then
		return `C.floor(x / p + .5) * p
	else
		return `C.floor(x + .5)
	end
end, glue.round)
low.snap = low.round

low.clamp = macro(function(x, m, M)
	return `min(max(x, m), M)
end, glue.clamp)

low.lerp = macro(function(x, x0, x1, y0, y1)
	return `[double](y0) + ([double](x)-[double](x0))
		* (([double](y1)-[double](y0)) / ([double](x1) - [double](x0)))
end, glue.lerp)

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

--other from glue...
low.pass = macro(glue.pass, glue.pass)
low.noop = macro(function() return quote end end, glue.noop)

--stdin/out/err --------------------------------------------------------------

--NOTE: we need to open new handles for these since Terra can't see the C
--global ones from stdio.h

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

local function format_arg(arg, fmt, args, freelist, indent)
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
			format_arg(`arg[i], fmt, args, freelist, indent+1)
			if i < t.N-1 then add(fmt, ',') end
		end
		add(fmt, ']')
	elseif t:isstruct() then
		local __tostring = t.metamethods.__tostring
		if __tostring then
			__tostring(arg, format_arg, fmt, args, freelist, indent)
		else
			add(fmt, tostring(t)..' {')
			local layout = t:getlayout()
			for i,e in ipairs(layout.entries) do
				add(fmt, '\n')
				add(fmt, ('   '):rep(indent+1))
				add(fmt, e.key..' = ')
				format_arg(`arg.[e.key], fmt, args, freelist, indent+1)
			end
			add(fmt, '\n')
			add(fmt, ('   '):rep(indent))
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
	format_arg(arg, fmt, args, freelist, 0)
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
			if out:setcapacity(32) then
				var n = snprintf(out.elements, out.capacity, fmt, [args])
				if n < 0 then
					out:free()
				elseif n < out.capacity then
					out.len = n+1
				else
					if not out:setcapacity(n+1) then
						out:free()
					else
						assert(snprintf(out.elements, out.capacity, fmt, [args]) == n)
						out.len = n+1
					end
				end
			end
			[ freelist ]
			in out
		end
	end
end, tostring)

--flushed printf -------------------------------------------------------------

low.pfn = macro(function(...)
	local args = {...}
	return quote
		var stdout = stdout()
		fprintf(stdout, [args])
		fprintf(stdout, '\n')
		fflush(stdout)
	end
end, function(...)
	print(string.format(...))
	io.stdout:flush()
end)

low.pf = macro(function(...)
	local args = {...}
	return quote
		var stdout = stdout()
		fprintf(stdout, [args])
		fflush(stdout)
	end
end, function(...)
	io.stdout:write(string.format(...))
	io.stdout:flush()
end)

--Lua-style print ------------------------------------------------------------

low.print = macro(function(...)
	local fmt, args, freelist = {}, {}, {}
	local n = select('#', ...)
	for i=1,n do
		local arg = select(i, ...)
		format_arg(arg, fmt, args, freelist, 0)
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
end, function(...)
	_G.print(...)
	io.stdout:flush()
end)

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
end, function(v, ...)
	if v then return v end
	if not ... then error('assertion failed', 2) end
	local t=pack(...); for i=1,t.n do t[i]=tostring(t[i]) end
	error(concat(t), 2)
end)

low.assertf = glue.assert

--clock ----------------------------------------------------------------------
--monotonic clock (can't go back or drift) in seconds with ~1us precision.

local clock
if Windows then
	extern('QueryPerformanceFrequency', {&int64} -> int32)
	extern('QueryPerformanceCounter',   {&int64} -> int32)
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
		assert(QueryPerformanceCounter(&t) ~= 0)
		return t * inv_qpf
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

--typed malloc ---------------------------------------------------------------

low.alloc = macro(function(T, len, oldp)
	oldp = oldp or `nil
	len = len or 1
	T = T:astype()
	return quote
		assert(len >= 0)
		var p = iif(len > 0, [&T](C.realloc(oldp, len * sizeof(T))), nil) in p
	end
end)

low.realloc = macro(function(p, len) --also works as free() when len = 0
	local T = assert(p:gettype().type, 'pointer expected, got ', p:gettype())
	return `alloc(T, len, p)
end)

--calls init() on all elements or otherwise zeroes the memory.
local empty = {}
low.new = macro(function(T, len, ...)
	len = len or 1
	T = T:astype()
	local init = T.getmethod and T:getmethod'init'
	if init then
		local init_args = #init.type.parameters > 1 and {...} or empty
		if len == 1 then
			return quote
				var p = alloc(T, len)
				p:init([init_args])
				in p
			end
		else
			return quote
				var p = alloc(T, len)
				for i=0,len do
					p[i]:init([init_args])
				end
				in p
			end
		end
	else
		return quote
			assert(len >= 0)
			var p = iif(len > 0, [&T](C.calloc(len, sizeof(T))), nil) in p
		end
	end
end)

low.memfree = macro(function(p, nilvalue)
	nilvalue = nilvalue or `nil
	return quote
		if p ~= nil then
			C.free(p)
			p = nilvalue
		end
	end
end)

--Note the necessity to pass a `len` if freeing an array of objects that
--have a free() method, and the necessity to use memfree() instead of free()
--if the buffer doesn't own the objects it contains but only holds a copy,
--or if inside the buffer's free() method if the buffer owns its memory.
--Normally a buffer doesn't own its memory, its container does, except for
--opaque handlers which allocate and free themselves with their own API.
--For those, call their own free method and nil the pointer manually instead.
low.free = macro(function(p, len, nilvalue)
	nilvalue = nilvalue or `nil
	len = len or 1
	local T = p:gettype().type
	local free = T.getmethod and T:getmethod'free'
	if free then
		if len == 1 then
			return quote
				if p ~= nil then
					p:free()
					C.free(p)
					p = nilvalue
				end
			end
		else
			return quote
				for i=0,len do
					p[i]:free()
				end
				C.free(p)
				p = nilvalue
			end
		end
	else
		return quote memfree(p, nilvalue) end
	end
end)

--typed memset ---------------------------------------------------------------

low.fill = macro(function(lval, val, len)
	if len == nil then --fill(lval, len)
		val, len = nil, val
	end
	val = val or 0
	len = len or 1
	local T = assert(lval:gettype().type, 'pointer expected, got ', lval:gettype())
	local size = sizeof(T)
	return quote
		assert(len >= 0)
		memset(lval, val, size * len)
		in lval
	end
end)

--typed memmove --------------------------------------------------------------

low.memcopy = macro(function(dst, src, len)
	len = len or 1
	local T1 = dst:gettype().type
	local T2 = src:gettype().type
	assert(sizeof(T1) == sizeof(T2), 'memcopy() sizeof mismatch ', T1, ' vs ', T2)
	local T = T1
	return quote memmove(dst, src, len * sizeof(T)) in dst end
end)

low.copy = macro(function(dst, src, len)
	--TODO: check if T1 can be cast to T2 or viceversa and use that instead!
	return `memcopy(dst, src, len)
end)

--typed memcmp ---------------------------------------------------------------

low.memequal = macro(function(p1, p2, len)
	len = len or 1
	local T1 = p1:gettype().type
	local T2 = p2:gettype().type
	assert(sizeof(T1) == sizeof(T2), 'memequal() sizeof mismatch ', T1, ' vs ', T2)
	local T = T1
	return `memcmp(p1, p2, len * sizeof(T)) == 0
end)

local op_eq = macro(function(a, b) return `@a == @b end)
local mt_eq = macro(function(a, b) return `a:__eq(b) end)

low.equal = macro(function(p1, p2, len)
	len = len or 1
	local T1 = p1:gettype().type
	local T2 = p2:gettype().type
	--TODO: check if T1 can be cast to T2 or viceversa first!
	assert(T1 == T2, 'equal() type mismatch ', T1, ' vs ', T2)
	local T = T1
	--check if elements must be compared via `==` operator.
	--floats can't be memcmp'ed since they may not be normalized.
	local must_op = T.metamethods and T.metamethods.__eq and op_eq
	local must_mt = T.getmethod and T:getmethod'__eq' and mt_eq
	local eq = must_op or must_mt or (T:isfloat() and op_eq)
	if eq then
		if len == 1 then
			return `eq(p1, p2)
		else
			return quote
				var equal = false
				for i=0,len do
					if eq(p1, p2) then
						equal = true
						break
					end
				end
				in equal
			end
		end
	else --fallback to memcmp
		return `memequal(p1, p2, len)
	end
end)

--default hash function ------------------------------------------------------

low.memhash = macro(function(size_t, k, h, len) --FNV-1A hash
	local size_t = size_t:astype()
	local T = assert(k:gettype().type, 'pointer expected, got ', k:gettype())
	local len = len or 1
	h = h and h ~= 0 and h or 0x811C9DC5
	return quote
		var d = [size_t](h)
		var k = [&int8](k)
		for i = 0, len * sizeof(T) do
			d = (d ^ k[i]) * 16777619
		end
		in d
	end
end)

low.hash = macro(function(size_t, k, h, len)
	len = len or 1
	h = h or 0
	local size_t = size_t:astype()
	local T = assert(k:gettype().type, 'pointer expected, got ', k:gettype())
	if T.getmethod then
		local method = '__hash'..(sizeof(size_t) * 8)
		if T:getmethod(method) then
			if len == 1 then
				return `k:[method]([size_t](h))
			else
				return quote
					var h = [size_t](h)
					for i=0,len do
						h = k:[method](h)
					end
					in h
				end
			end
		end
	end
	return `memhash(size_t, k, h, len)
end)

--readfile -------------------------------------------------------------------

local terra readfile(name: rawstring): {&opaque, int64}
	var f = fopen(name, 'rb')
	defer fclose(f)
	if f ~= nil then
		if fseek(f, 0, SEEK_END) == 0 then
			var filesize = ftell(f)
			if filesize > 0 then
				rewind(f)
				var out = [&opaque](alloc(uint8, filesize))
				if out ~= nil and fread(out, 1, filesize, f) == filesize then
					return out, filesize
				end
				C.free(out)
			end
		end
	end
	return nil, 0
end
low.readfile = macro(function(name) return `readfile(name) end, glue.readfile)

--freelist -------------------------------------------------------------------

low.freelist = memoize(function(T)
	local struct freelist {
		next: &freelist;
	}
	addmethods(freelist, function()
		assert(sizeof(T) >= sizeof(&opaque), 'freelist item too small')
		terra freelist:init()
			self.next = nil
		end
		terra freelist:free()
			while self.next ~= nil do
				var next = self.next.next
				C.free(self.next)
				self.next = next
			end
		end
		terra freelist:alloc()
			if self.next ~= nil then
				var p = self.next
				self.next = p.next
				return [&T](p)
			else
				return alloc(T)
			end
		end
		terra freelist:new()
			var p = self:alloc()
			return iif(p ~= nil, fill(p), nil)
		end
		terra freelist:release(p: &T)
			var node = [&freelist](p)
			node.next = self.next
			self.next = node
		end
	end)
	return freelist
end)

--building to dll for LuaJIT ffi consumption ---------------------------------

--Features:
-- * supports publishing terra functions and named terra structs with methods.
-- * tuples and function pointers are typedef'ed with friendly unique names.
-- * the same tuple definition can appear in multiple modules without error.
-- * auto-assign methods to types via ffi.metatype.
-- * enable getters and setters via ffi.metatype.
-- * type name override with `__typename_ffi` metamethod.
-- * deciding which methods to publish via `public_methods` table.
-- * deciding which methods to publish via `__ismethodpublic` metamethod.
-- * publishing enum and bitmask values.
-- * diff-friendly deterministic output.

function low.publish(modulename)

	local self = {}
	setmetatable(self, self)

	local objects = {}
	local enums = {}

	function self:__call(T, public_methods, opaque)
		if type(T) == 'terrafunction' or (T:isstruct() and not istuple(T)) then
			T.opaque = opaque
			T.public_methods = public_methods
			add(objects, T)
		elseif type(T) == 'table' and not getmetatable(T) then --plain table: enums
			update(enums, T)
		else
			assert(false, 'expected terra function, struct or enum table, got ', T)
		end
		return T
	end

	function self:getenums(moduletable)
		for k,v in pairs(moduletable) do
			if type(k) == 'string' and type(v) == 'number' and k:upper() == k then
				enums[k] = v
			end
		end
	end

	local saveobj_table = {}

	function self:bindingcode()
		local tdefs = {} --typedefs
		local pdefs = {} --function pointer typedefs
		local xdefs = {} --shared struct defs (pcalled)
		local cdefs = {} --function defs
		local mdefs = {} --metatype defs

		add(tdefs, "local ffi = require'ffi'\n")
		add(tdefs, "local C = ffi.load'"..modulename.."'\n")
		add(tdefs, 'ffi.cdef[[\n')

		local ctype --fw. decl.

		local function cdef_tuple(T, name)
			add(xdefs, 'pcall(ffi.cdef, \'')
			append(xdefs, 'struct ', name, ' { ')
			for i,e in ipairs(T.entries) do
				local name, type = e[1], e[2]
				append(xdefs, ctype(type), ' ', name, '; ')
			end
			add(xdefs, '};\')\n')
		end

		local function typedef_struct(name)
			append(tdefs, 'typedef struct ', name, ' ', name, ';\n')
		end

		local function typedef_functionpointer(T, name)
			append(pdefs, 'typedef ', ctype(T.returntype), ' (*', name, ') (')
			for i,arg in ipairs(T.parameters) do
				add(pdefs, ctype(arg))
				if i < #T.parameters then
					add(pdefs, ', ')
				end
				if T.isvararg then
					add(pdefs, ',...')
				end
			end
			add(pdefs, ');\n')
		end

		local function append_typename_fragment(s, T, n)
			if not T then return s end
			local ct = T:isintegral() and tostring(T):gsub('32$', '')
				or T:ispointer() and 'p' .. ctype(T.type)
				or ctype(T)
			return s .. (s ~= '' and  '_' or '') .. ct .. (n > 1 and n or '')
		end
		local function unique_typename(types)
			local type0, n = nil, 0
			local s = ''
			for i,type in ipairs(types) do
				if type ~= type0 then
					s = append_typename_fragment(s, type0, n)
					type0, n = type, 1
				else
					n = n + 1
				end
			end
			return append_typename_fragment(s, type0, n)
		end

		local function tuple_typename(T)
			--each tuple entry is the array {name, type}, hence plucking index 2.
			return unique_typename(glue.map(T.entries, 2))
		end

		local function function_typename(T)
			local s = unique_typename(T.parameters)
			s = s .. (s ~= '' and '_' or '') .. 'to'
			return append_typename_fragment(s, T.returntype, 1)
		end

		local function clean_typename(s)
			return (s:gsub('[%${},()]', '_'))
		end

		local function typename(T, name)
			local typename = T.metamethods and T.metamethods.__typename_ffi
			local typename = typename
				and (type(typename) == 'string' and typename or typename(T))
			if istuple(T) then
				typename = typename or clean_typename(tuple_typename(T))
				cdef_tuple(T, typename)
				typedef_struct(typename)
			elseif T:isstruct() then
				typename = typename or clean_typename(tostring(T))
				typedef_struct(typename)
			elseif T:isfunction() then
				typename = typename or clean_typename(name or function_typename(T))
				typedef_functionpointer(T, typename)
			else
				typename = clean_typename(tostring(T))
			end
			return typename
		end
		typename = memoize(typename)

		function ctype(T)
			if T:isintegral() then
				return tostring(T)..'_t'
			elseif T:isfloat() or T:islogical() then
				return tostring(T)
			elseif T == rawstring then
				return 'const char *'
			elseif T:ispointer() then
				if T:ispointertofunction() then
					return typename(T.type, T.__typename_ffi)
				else
					return ctype(T.type)..'*'
				end
			elseif T == terralib.types.opaque or T:isunit() then
				return 'void'
			elseif T:isstruct() then
				return typename(T)
			else
				assert(false, 'NYI: ', tostring(T), T:isarray())
			end
		end

		local function cdef_function(func, name)
			local T = func:gettype()
			append(cdefs, ctype(T.returntype), ' ', name, '(')
			for i,arg in ipairs(T.parameters) do
				add(cdefs, ctype(arg))
				if i < #T.parameters then
					add(cdefs, ', ')
				end
				if T.isvararg then
					add(cdefs, ',...')
				end
			end
			add(cdefs, ');\n')
			saveobj_table[name] = func
		end

		local function cdef_entries(cdefs, entries, indent)
			for i,e in ipairs(entries) do
				for i=1,indent do add(cdefs, '\t') end
				if #e > 0 and type(e[1]) == 'table' then --union
					add(cdefs, 'union {\n')
					cdef_entries(cdefs, e, indent + 1)
					for i=1,indent do add(cdefs, '\t') end
					add(cdefs, '};\n')
				else
					append(cdefs, ctype(e.type), ' ', e.field, ';\n')
				end
			end
		end
		local function cdef_struct(T)
			name = typename(T)
			if not T.opaque then
				append(cdefs, 'struct ', name, ' {\n')
				cdef_entries(cdefs, T.entries, 1)
				add(cdefs, '};\n')
			end
		end

		local function ispublic(T, fname)
			return not T.public_methods or T.public_methods[fname]
		end

		local function cdef_methods(T)
			local function cmp(k1, k2) --declare methods in source code order
				local d1 = T.methods[k1].definition
				local d2 = T.methods[k2].definition
				if d1.filename == d2.filename then
					return d1.linenumber < d2.linenumber
				else
					return d1.filename < d2.filename
				end
			end
			local ispublic = T.metamethods.__ismethodpublic or ispublic
			local name = typename(T)
			local methods = {}
			local getters = {}
			local setters = {}
			local terramethods = glue.map(T.methods, function(k, v)
				return type(v) == 'terrafunction' and v or nil
			end)
			for fname, func in sortedpairs(terramethods, cmp) do
				if ispublic(T, fname) then
					local cname = name..'_'..fname
					cdef_function(func, cname)
					local t
					if T.gettersandsetters then
						if fname:starts'get_' and #func.type.parameters == 1 then
							t, fname = getters, fname:gsub('^get_', '')
						elseif fname:starts'set_' and #func.type.parameters == 2 then
							t, fname = setters, fname:gsub('^set_', '')
						else
							t = methods
						end
					else
						t = methods
					end
					add(t, '\t'..fname..' = C.'..cname..',\n')
				end
			end
			if T.gettersandsetters and (#getters > 0 or #setters > 0) then
				add(mdefs, 'local getters = {\n'); extend(mdefs, getters); add(mdefs, '}\n')
				add(mdefs, 'local setters = {\n'); extend(mdefs, setters); add(mdefs, '}\n')
				add(mdefs, 'local methods = {\n'); extend(mdefs, methods); add(mdefs, '}\n')
				add(mdefs, [[
ffi.metatype(']]..name..[[', {
	__index = function(self, k)
		local getter = getters[k]
		if getter then return getter(self) end
		return methods[k]
	end,
	__newindex = function(self, k, v)
		local setter = setters[k]
		if not setter then
			error(('field not found: %s'):format(tostring(k)), 2)
		end
		setter(self, v)
	end,
})
]])
			else
				append(mdefs, 'ffi.metatype(\'', name, '\', {__index = {\n')
				extend(mdefs, methods)
				add(mdefs, '}})\n')
			end
		end

		for i,obj in ipairs(objects) do
			if type(obj) == 'terrafunction' then
				cdef_function(obj, obj.name)
			elseif obj:isstruct() then
				cdef_struct(obj)
				cdef_methods(obj)
			end
		end

		add(cdefs, ']]\n')

		local enums_code = ''
		if next(enums) then
			local t = {'ffi.cdef[[\nenum {\n'}
			local function addt(s) add(t, s) end
			for k,v in sortedpairs(enums) do
				add(t, '\t'); add(t, k); add(t, ' = '); pp.write(addt, v); add(t, ',\n')
			end
			enums_code = concat(t) .. '}]]\n'
		end

		return concat(tdefs) .. concat(pdefs) .. concat(cdefs)
			.. concat(xdefs) .. concat(mdefs) .. enums_code
			.. 'return C\n'

	end

	function self:savebinding()
		zone'savebinding'
		local filename = modulename .. '_h.lua'
		writefile(filename, self:bindingcode(), nil, filename..'.tmp')
		zone()
	end

	function self:binpath(filename)
		return 'bin/'..platform..(filename and '/'..filename or '')
	end

	function self:objfile()
		return self:binpath(modulename..'.o')
	end

	function self:saveobj()
		zone'saveobj'
		terralib.saveobj(self:objfile(), 'object', saveobj_table)
		zone()
	end

	function self:removeobj()
		os.remove(self:objfile())
	end

	function self:linkobj(linkto)
		zone'linkobj'
		local soext = {Windows = 'dll', OSX = 'dylib', Linux = 'so'}
		local sofile = self:binpath(modulename..'.'..soext[ffi.os])
		local linkargs = linkto and '-l'..concat(linkto, ' -l') or ''
		local cmd = 'gcc '..self:objfile()..' -shared '..'-o '..sofile
			..' -L'..self:binpath()..' '..linkargs
		os.execute(cmd)
		zone()
	end

	function self:build(opt)
		opt = opt or {}
		self:savebinding()
		self:saveobj()
		self:linkobj(opt.linkto)
		self:removeobj()
	end

	return self
end

return low
