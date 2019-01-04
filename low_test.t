
local ffi = require'ffi'
local low = require'low'
setfenv(1, low.C)

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
	var stack: low.stack(int32)
	stack:alloc(1)
	stack:push(5)
	stack:pop()
end
test_stack()

