#ifdef USE_CUDA
#define MY_ROUTINE(x)  x##_gpu
#else
#define MY_ROUTINE(x)  x##_cpu
#endif

subroutine MY_ROUTINE(vector_add)(a,b,N)
  real:: a(*)
  real:: b
  integer:: i,N
#ifdef USE_CUDA
  attributes(device):: a
#endif

!$cuf kernel do(1) <<<*,*>>>
   do i=1,N
     a(i)=a(i)+b
   end do
  
end subroutine MY_ROUTINE(vector_add)
