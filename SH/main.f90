program testsh
implicit none
INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
integer:: lmaxq=5,ngm=264550,i,loc
real(DP), allocatable :: ylmk0(:,:),g(:,:),gg(:)
real(DP):: rand
real(DP):: t1,t2,wallclock
!real:: t1,t2,time

allocate(ylmk0(ngm,lmaxq*lmaxq))
allocate(g(3,ngm),gg(ngm))


do i=1,ngm
  g(1,i)=rand()
  g(2,i)=rand()
  g(3,i)=rand()
  gg(i)=g(1,i)*g(1,i)+ g(2,i)*g(2,i)+ g(3,i)*g(3,i)
end do

!call cpu_time(t1)
t1=wallclock()
CALL ylmr2 (lmaxq * lmaxq, ngm, g, gg, ylmk0)
t2=wallclock()
!call cpu_time(t2)

print *,"length of the vector=",ngm
print *,"SH time=",t2-t1
loc=1
do i=1,25
 print *,"ylmk0",i,ylmk0(loc,i)
end do

deallocate(ylmk0,g,gg)
end program testsh
