!-------------------------------------!
! Author: Antonio De Nicola           ! 
!                                     !
! Last Mod.: 2024.17.09               !
! Contacts: adenicola.chem@gmail.com  !
!-------------------------------------!
      program properties 
#ifdef _OPENMP
      use omp_lib
#endif
      implicit none

      real*8 vbond, vangle, vcosa, ea, eb, esa, esb, dmy
      real*8 epot, etot
      real*8 vscf1, vscf2, wr1, wr2, wr3, wr4, ekin, temp
      integer i, j, k, ncpu, frms, time_frm, numa, nfree
      integer n, var, dm1, f, stat
      real*8, allocatable:: prep(:) 
      real*8, parameter:: gascon=8.3143d-3
      integer, parameter:: prd=6000
      logical :: check_f15  


       print *, "-------------------------------------"
       print *, "        PROPERTIES COLLECTOR "
       print *, "                 FOR"
       print *, "       OCCAM PARALLEL CPU Ver."
       print *, "-------------------------------------"
       print *, ""
       print *, ""
#ifdef _OPENMP
       print *, " OpenMP execution, ver.", _OPENMP
#else  
       print *, " Serial execution."
#endif

!### CHECK IF FORT.15 EXIST
!### as alternative, ncpu is required as input 
      open(unit=15, file='fort.15', status='old', action='read', iostat=var)
      if (var.eq.0) then
          n = 0
          do while (var==0)
            read(15,*, IOSTAT=var)
            n = n + 1
          enddo 
          print *, ""
          print *, "-------------------------------------"
          ncpu = n - 1
          print *, "Detected No. of CPUs:", ncpu
      elseif (var.ne.0)then         
          print *, ""
          print *, "-------------------------------------"
          print *, "fort.15 is not found !!!"
          print *, "please insert no. of CPU:"
          read(5,*) ncpu
      endif

!### COUNTING No. OF FRAMES
      open (unit=6001, file='fort.6001', status='old', action='read', iostat=stat)
    
      f = 0
      do while (stat==0)
         read(6001, *, IOSTAT=stat)
         f = f + 1
      enddo       
      rewind(6001)
      frms = (f - 3)/51
!### Note: is assumed that fort.6xxx contains a list of 50 partial properties values.
!###       If such condition is changed, automatic calculation of frames will be wrong !!!
      print *, "Detected No. of frames: ", frms
      print *, "-------------------------------------"

!### VECTORORS and VARIABLES INIT.
      open (unit=7, file='properties.dat', status='unknown')

      allocate(prep(50))
      do i = 1, 50
         prep(i) = 0.0d00
      enddo

      time_frm   = 0
      vbond      = 0.0d00
      vangle     = 0.0d00
      vcosa      = 0.0d00
      vscf1      = 0.0d00
      vscf2      = 0.0d00
      ekin       = 0.0d00
      temp       = 0.0d00
      ea         = 0.0d00
      eb         = 0.0d00
      esb        = 0.0d00
      esa        = 0.0d00
      wr1        = 0.0d00
      wr2        = 0.0d00
      wr3        = 0.0d00
      wr4        = 0.0d00
      epot       = 0.0d00
      etot       = 0.0d00    
 
!### READING MAIN LOOP OVER FRMS

      do k = 1, frms
!### VECTOR RE-INIT.
         do i = 1, 50
            prep(i) = 0.0d00
         enddo
!### LOOP OVER CPUs
         do i = 1, ncpu !No. of CPUs
            if(k.le.1) then
               read(prd+i,*) dm1, numa
               read(prd+i,*)
               read(prd+i,*) time_frm
               nfree = 3*numa
            else
               read(prd+i,*) time_frm
            endif            

!### LOOP OVER 50 properties output in fort.6xxx files
#ifdef _OPENMP
!omp parallel do private(j, dmy) &
!omp reduction(+:prep)
!omp scheduled(guided) 
            do j = 1, 50
               read(prd+i,*) dmy
               prep(j) = prep(j) + dmy
            enddo ! CLOSE LOOP Properties
!omp end parallel do
#else
            do j = 1, 50
               read(prd+i,*) dmy
               prep(j) = prep(j) + dmy
            enddo ! CLOSE LOOP Properties

#endif
         enddo ! CLOSE LOOP CPU

!### WRITING PROPERTIES MAIN LOOP
!### VARIABLES ASSIGNMENT PER FRAME
         vbond     = prep(1)
         vangle    = prep(8)
         vcosa     = prep(15)

         ea       = prep(28)
         esa      = prep(29)
         eb       = prep(30)
         esb      = prep(31)

         vscf1    = prep(35)
         vscf2    = prep(36)

         wr1      = prep(37)
         wr2      = prep(38)
         wr3      = prep(39)
         wr4      = prep(40)

         ekin     = prep(50)
!### END VARIABLE ASSIGNMENT

!### CALCULATION OF PROPERTIES

        wr1 = wr1/ncpu
        wr2 = wr2/ncpu
        wr3 = wr3/ncpu
        wr4 = wr4/ncpu

        epot=ea+eb+vbond+vangle+vscf1+vscf2
        etot=ekin+epot
        temp = (2.0d00 * ekin) / (nfree * gascon)

!### WRITING PROPERTIES
        write(7,*) 'step no.  ' , time_frm
        write(7,*) time_frm, vbond, 'vbond'
        write(7,*) time_frm, vcosa, 'vangle_cos'
        write(7,*) time_frm, vangle, 'vangle'
        write(7,*) time_frm, eb, 'nonbond inter'
        write(7,*) time_frm, ea, 'nonbond intra'
        write(7,*) time_frm, ea+eb, 'nonbond total'
        write(7,*) time_frm, vscf1, 'vscf1'
        write(7,*) time_frm, vscf2, 'vscf2'
        write(7,*) time_frm, vscf1+vscf2, 'v_scf'
        write(7,*) time_frm, wr1+wr2  , ' W[phi(r)]'
        write(7,*)time_frm,ea+eb+vscf1+vscf2, 'nonbond tot + mean field'
        write(7,*) time_frm, epot , 'epot non-shifted'
        write(7,*) time_frm, epot , 'epot shifted'
        write(7,*) time_frm, etot , 'etot non-shifted'
        write(7,*) time_frm, temp , 'temp'
      enddo ! CLOSE LOOP FRMS

      print *, ""
      print *, "OUTPUT: properties.dat "

      end program
      
