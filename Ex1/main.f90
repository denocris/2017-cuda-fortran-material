program example_vector_add
real, allocatable:: a(:)
integer:: i, N

N=32
allocate(a(N))

a=1.
call vector_add(a,10.,N)

if(any(a /= 11.) )  then
  print *,"Vector add failed"
else
  print *,"Vector add passed"
end if 
end program example_vector_add
