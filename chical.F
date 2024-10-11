       program chical
       implicit none
       
       integer i, nlj, tmp1, tmp2, tmp3, dm1, statA, statB
       real e1, e2, e3, chi
       real,allocatable:: sig(:), eps(:)
       character*6,allocatable:: A(:), B(:)
       character*6, inA, inB

   
       nlj = 355746
       chi = 0.d00

       open(unit=3, file='param.dat')

       allocate(sig(nlj))
       allocate(eps(nlj))
       allocate(A(nlj))
       allocate(B(nlj))

       write(6,*) 'type i:'
       read(5,*) inA
       write(6,*) 'type j:'
       read(5,*) inB

       statA = 0
       statB = 0
       read(3,*)
       do i = 1, nlj
          read(3,*) A(i), B(i), dm1, sig(i), eps(i)
          if(inA.eq.A(i).or.inA.eq.B(i))then
              statA = statA + 1
          endif
          if(inB.eq.A(i).or.inB.eq.B(i))then
              statB = statB + 1
          endif
       enddo

       if(statA.eq.0)then
           write(6,*) '* * * ERROR * * *'
           write(6,*) 'Type ', inA, ' is missing'
           stop
           go to 10
       endif    

       if(statB.eq.0)then
           write(6,*) '* * * ERROR * * *'
           write(6,*) 'Type ', inB, ' is missing'
           stop
           go to 10
       endif    


       !inA = 'P6'
       !inB = 'N1dq'

       do i = 1, nlj
          if(A(i).eq.inA.and.B(i).eq.inA)then
             tmp1 = i 
          elseif(A(i).eq.inA.and.B(i).eq.inB.or. 
     $           A(i).eq.inB.and.B(i).eq.inA)then !mix
             tmp2 = i 
          elseif(A(i).eq.inB.and.B(i).eq.inB)then
             tmp3 = i 
          endif
       enddo


       e1 = -1.0*eps(tmp1)
       e2 = -1.0*eps(tmp2)
       e3 = -1.0*eps(tmp3)
       chi = 3.0d00 * ( 2*e2 - (e1+e3) )

       write(6,*) '---------------------------'
       write(6,*) 'Martini LJ param [kJ/mol]  '
       write(6,*) '---------------------------'
       write(6,*) A(tmp1), B(tmp1), eps(tmp1)
       write(6,*) A(tmp2), B(tmp2), eps(tmp2)
       write(6,*) A(tmp3), B(tmp3), eps(tmp3)
       write(6,*) '---------------------------'
       write(6,*) 'i, j, [int.param.]*RT'
       write(6,*) inA, inB, chi

10    end program   

