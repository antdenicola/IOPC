C-----------------------------------------------------------------------------C
! This program IOPC generates the input files and organize output files for   !
! the paralle version of OCCAM. The program works with fort.5                 !
!                                                                             !
!                                                                             !
! Written by: Ying Zhao and Antonio De Nicola                                 !
! Version 1.7                                                                 !
! develope time 07.10.2010                                                    !
! modify time 29.11.2010                                                      !  
! modify time 08.05.2011                                                      !
! modify time 20.07.2011                                                      !
! modify time 05.08.2011                                                      !
! modify time 28.11.2011                                                      !
! modify time 12.01.2012                                                      !
! Contacts: adenicola.chem@gmail.com - zhaoyingmpi2006@gmail.com              !
C-----------------------------------------------------------------------------C

      Program  main 
      use varsh
      implicit none 

      write(6,*) '========================================'
      write(6,*) '       * * * I. O. P. C. * * *          '
      write(6,*) ' Input Program for Parallel Occam Code  '
      write(6,*) ' ver. 1.7                               '
      write(6,*) '========================================'
      write(6,*)
10    write(6,*) '> insert [1] for input program'
      write(6,*) '> insert [2] for output program'
      write(6,*) '> insert [3] for help'
      read(5,*) mode

      if (mode.eq.1)then

        call ofile
 
        if (istop.gt.0) then
          write(6,*)' *** FATAL ERROR IN THE I/O FILE ***'
          write(6,*)'    please check your input files   '
          stop
        endif

        call input 
   
       if (vmode.eq.1)then
      
        call rdconf_vel

        call wrconf_vel

       elseif (vmode.eq.2)then

        call rdconf

        call wrconf

       endif


       write(6,*)'---------------------------------------'
       write(6,*)'> End Input Program'

      elseif(mode.eq.2)then

        call wtrj

        write(6,*)'> End Output Program'

      elseif(mode.eq.3)then
        write(6,*)
        write(6,*)'+ Help -------------------------------------------+'
        write(6,*)'| For the input you need of fort.10 (coord.)      |' 
        write(6,*)'| For the output you need of fort.20i and fort.15 |'
        write(6,*)'+-------------------------------------------------+'
        write(6,*)
        go to 10
      elseif(mode.gt.3)then
        write(6,*)'Your choose is not valid, retry'
        go to 10
      endif

  

      end program
