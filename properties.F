!-------------------------------------!
! Author: Antonio De Nicola           ! 
!                                     !
! Modify: 2019.10.29                  !
! Contacts: adenicola.chem@gmail.com  !
!-------------------------------------!
      program properties 
      implicit none

      integer i, j, k, ncpu, frms, prd
      real*8,allocatable:: vscf1(:,:), vscf2(:,:), v1, v2
      integer,allocatable:: step(:)
      real*8 dmy

    
      print *, "Insert no. of CPU:"
      read(5,*) ncpu
      print *, "Inser no. of frames:"
      read(5,*) frms
      prd = 6000

      allocate(vscf1(ncpu, frms))
      allocate(vscf2(ncpu, frms))
      allocate(step(frms))

      do i = 1, ncpu
        do k = 1, frms
         vscf1(i,k) = 0.0d00
         vscf2(i,k) = 0.0d00
         step(k) = 0
        enddo
      enddo


!### READING
      do i = 1, ncpu
        read(prd+i,*) 
        read(prd+i,*) 
        do k=1, frms
          read(prd+i,*) step(k)
          do j = 1, 50
            read(prd+i,*) dmy
            if(j.eq.35)then
              vscf1(i,k) = vscf1(i,k) + dmy
            endif
            if(j.eq.36)then
              vscf2(i,k) = vscf2(i,k) + dmy
            endif
          enddo
        enddo
      enddo

!### WRITING
      write(7,*) "#step,     vscf1,      vscf2"
      do k = 1, frms
         v1 = 0.0d00
         v2 = 0.0d00
         do i = 1, ncpu
!            write(7,*) step(k), vscf1(i,k), i, k
            v1 = v1 + vscf1(i,k) 
            v2 = v2 + vscf2(i,k) 
         enddo
        write(7,*) step(k), v1, v2
      enddo

      end program
      
