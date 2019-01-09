
local ffi = require'ffi'
setfenv(1, require'low'.C)

local function test_pr()
	local S = struct { x: int; y: int; }
	local f = {} -> {}
	local terra test(): {}
		var a: int[5] = array(1, 2, 3, 4, 5)
		var p: &int = a
		var s = S { 5, 7 }
		pr('hello', 'world', 42)
		pr([int8](-5), [uint8](5), [int16](-7), [uint16](7))
		pr([int32](-2), [uint32](2))
		pr([double](3.2e-100), [float](-2.5))
		pr(-25LL, 0xffffULL)
		pr(p, a, s, f)
	end
	test()
end
test_pr()

local cmp = macro(function(t, i, v) return `t[i] <= v end)
local terra test_binsearch()
	var a = arrayof(int, 11, 13, 15)
	var i = 0
	var j = 2
	check(binsearch(10, a, 0,-1) ==  0)
	check(binsearch(10, a, 0, 0) ==  0)
	check(binsearch(11, a, 0, 0) ==  0)
	check(binsearch(12, a, 0, 0) ==  1)
	check(binsearch(12, a, 0, 1) ==  1)
	check(binsearch(13, a, 0, 1) ==  1)
	check(binsearch(11, a, 0, 1) ==  0)
	check(binsearch(14, a, 0, 1) ==  2)
	check(binsearch(10, a, 0, 1) ==  0)
	check(binsearch(14, a, i, j) ==  2)
	check(binsearch(12, a, i, j) ==  1)
	check(binsearch(10, a, i, j) ==  0)
	check(binsearch(16, a, i, j) ==  3)
end
test_binsearch()

local terra test_stack()
	var stack: stack(int32)
	stack:alloc(1)
	stack:push(5)
	stack:pop()
end
test_stack()

local terra test_freelist()
	var fl: freelist(int32)
	fl:alloc(10)
	var p = fl:new()
	check(fl.len == 0)
	fl:release(p)
	check(fl.len == 1)
	var p2 = fl:new()
	check(fl.len == 0)
	check(p == p2)
	fl:release(p2)
	fl:free()
end
test_freelist()

local terra test_growbuffer()
	var buf: growbuffer(int)
	buf:alloc()
	check(buf.data == nil)
	do
		var buf = buf(20)
		check(buf ~= nil)
	end
	buf:free()
end
test_growbuffer()

