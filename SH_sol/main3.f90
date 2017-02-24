program testsh
#ifdef USE_CUDA
use ylmr2_gpu
use cudafor
#endif
implicit none
INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
integer:: lmaxq=5,ngm=264550,i,loc
!integer:: lmaxq=5,ngm=64550,i,loc
real(DP), allocatable :: ylmk0(:,:),g(:,:),gg(:),ylmk0_ref(:,:)
#ifdef USE_CUDA
real(DP), allocatable,device :: ylmk0_d(:,:),g_d(:,:),gg_d(:)
type (cudaEvent) :: startEvent, stopEvent ! events for timing
integer:: istat
#endif
real(DP):: rand
real:: t1,t2,time

allocate(ylmk0(ngm,lmaxq*lmaxq))
allocate(ylmk0_ref(ngm,lmaxq*lmaxq))
allocate(g(3,ngm),gg(ngm))


do i=1,ngm
  g(1,i)=rand()
  g(2,i)=rand()
  g(3,i)=rand()
  gg(i)=g(1,i)*g(1,i)+ g(2,i)*g(2,i)+ g(3,i)*g(3,i)
end do

loc=1
call cpu_time(t1)
CALL ylmr2 (lmaxq * lmaxq, ngm, g, gg, ylmk0)
call cpu_time(t2)
print *,"SH time",t2-t1
do i=1,25
print *,"ylmk0",i,ylmk0(loc,i)
end do

ylmk0_ref=ylmk0
ylmk0=0.
#ifdef USE_CUDA
istat = cudaEventCreate(startEvent)
istat = cudaEventCreate(stopEvent)
allocate(ylmk0_d(ngm,lmaxq*lmaxq))
allocate(g_d(3,ngm),gg_d(ngm))
g_d=g
gg_d=gg
ylmk0_d=0.

istat = cudaEventRecord(startEvent, 0)
CALL ylmr2_gpu (lmaxq * lmaxq, ngm, g_d, gg_d, ylmk0_d)
istat = cudaEventRecord(stopEvent, 0)
istat = cudaEventSynchronize(stopEvent)

istat = cudaEventElapsedTime(time, startEvent, stopEvent)
print *,"CUDA version (Cuda Fortran ) time=",time*10.**(-3)
ylmk0=ylmk0_d

deallocate(ylmk0_d,g_d,gg_d)
#else
call cpu_time(t1)
CALL ylmr2_cpu (lmaxq * lmaxq, ngm, g, gg, ylmk0)
call cpu_time(t2)
print *,"new SH time",t2-t1
#endif
do i=1,25
print *," GPU ylmk0",i,ylmk0(loc,i),ylmk0(loc,i)-ylmk0_ref(loc,i)
end do
deallocate(ylmk0,g,gg)
end program testsh
