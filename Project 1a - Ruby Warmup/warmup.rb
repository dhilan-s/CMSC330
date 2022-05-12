def fib(n)
    nums = Array.new
	twobefore = 0
	onebefore = 1
	curr = 0
	i = 0
	while i <= n
		if i == 1
			nums << twobefore
		end
		if i == 2
			nums << onebefore
		end
		if i > 2
			curr = twobefore + onebefore
			nums << curr
			twobefore = onebefore
			onebefore = curr
		end
		i = i + 1
	end
	return nums
end

def isPalindrome(n)
    s = n.to_s
	return s == s.reverse
end

def nthmax(n, a)
	sorted = a.sort!
	if sorted.length - n > 0
		max = sorted[sorted.length-1 - n]
	else
		max = nil
	end
	return max
end

def freq(s)
    letters = Hash.new(0)
	s.each_char do |curr|
		letters[curr] = letters[curr]+1
	end
	vals = letters.values
	maxi = vals.max
	mode = letters.key(maxi)
	return mode.to_s
end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length
		return nil
	else
		h = Hash.new
		i = 0
        while i < arr1.length do
            h[arr1[i]] = arr2[i]
			i = i + 1
        end
	end
	return h
end

def hashToArray(hash)
    hashes = Array.new {Array.new}
	hash.each do |key, value|
		hashes.push([key,value])
	end
	return hashes
end
