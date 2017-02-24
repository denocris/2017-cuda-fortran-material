program example_vector_add
real, allocatable:: a(:)
real, allocatable,device:: a_d(:)
integer:: i, N

N=32
allocate(a(N))

a=1.
allocate(a_d,source=a)

print *,"doing sum on cpu"
call vector_add_cpu(a,10.,N)


if(any(a /= 11.) )  then
  print *,"Vector add on CPU failed"
  print *,a
else
  print *,"Vector add on CPU passed"
  print *,a
end if 

print *,"doing sum on gpu"
a=0.
call vector_add_gpu(a_d,10.,N)
a=a_d

if(any(a /= 11.) )  then
  print *,"Vector add on GPU  failed"
  print *,a
else
  print *,"Vector add  on GPU passed"
  print *,a
end if 

end program example_vector_add
