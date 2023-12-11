package euler

import "slices"

func factorNaive(n int) int {
	factors := []int{}
	k := 2
	for k <= n {
		for n%k == 0 {
			factors = append(factors, k)
			n /= k
		}
		k += 1
	}
	return slices.Max(factors)
}

func factorOpt(n int) int {
	k := 3
	for n%2 == 0 {
		n /= 2
	}
	for k*k <= n && n > 1 {
		for n%k == 0 {
			n /= k
		}
		k += 2
	}
	if n > 1 {
		return n
	} else {
		return k
	}
}
