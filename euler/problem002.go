package euler

func fibImp(limit int) int {
	a := 1
	b := 1
	s := 0
	for b <= limit {
		if b%2 == 0 {
			s += b
		}
		a, b = b, a+b
	}
	return s
}

func fibOpt(limit int) int {
	a := 1
	b := 1
	c := 2
	s := 0
	for c <= limit {
		s += c
		a = b + c
		b = a + c
		c = a + b
	}
	return s
}
