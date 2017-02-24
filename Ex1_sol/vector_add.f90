subroutine vector_add(a,b,N)
  real:: a(*)
  real:: b
  integer:: i,N

  print *,"N=",N
   do i=1,N
     a(i)=a(i)+b
   end do
  
end subroutine vector_add

