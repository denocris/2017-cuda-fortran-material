subroutine vector_add_cpu(a,b,N)
  real:: a(*)
  real:: b
  integer:: i,N

   do i=1,N
     a(i)=a(i)+b
   end do
  
end subroutine vector_add_cpu

subroutine vector_add_gpu(a,b,N)
  real,device:: a(*)
  real:: b
  integer:: i,N

!$cuf kernel do(1) <<<*,*>>>
   do i=1,N
     a(i)=a(i)+b
   end do
  
end subroutine vector_add_gpu
