function fibIter(limit)
  a = 0
  b = 1
  acc = 0
  while (a + b) <= limit
    next = a + b
    if iseven(next)
      acc += next
    end
    a, b = b, a+b
  end
  acc
end

function fibOpt(limit)
  a = 1
  b = 1
  c = 2
  acc = 0
  while c <= limit
    acc += c
    a = b + c
    b = a + c
    c = a + b
  end
  acc
end
