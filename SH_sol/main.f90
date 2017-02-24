program testsh
implicit none
INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
integer:: lmaxq=5,ngm=264550,i,loc
!integer:: lmaxq=5,ngm=64550,i,loc
real(DP), allocatable :: ylmk0(:,:),g(:,:),gg(:),ylmk0_ref(:,:)
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

call cpu_time(t1)
CALL ylmr2 (lmaxq * lmaxq, ngm, g, gg, ylmk0)
call cpu_time(t2)
print *,"SH time",t2-t1

loc=1
do i=1,25
print *,"ylmk0",i,ylmk0(loc,i)
end do

ylmk0_ref=ylmk0
ylmk0=0.
call cpu_time(t1)
CALL ylmr2_cpu (lmaxq * lmaxq, ngm, g, gg, ylmk0)
call cpu_time(t2)
print *,"new SH time",t2-t1

print *," Compare new results to original ones"
do i=1,25
print *," new ylmk0",i,ylmk0(loc,i),ylmk0(loc,i)-ylmk0_ref(loc,i)
end do
deallocate(ylmk0,g,gg)
end program testsh
