       program chimat
       implicit none
       
       integer i, j, k, nlj, tmp1, tmp2, tmp3, dm1, statA, statB
       integer nseq
       real e1, e2, e3, chi
       real,allocatable:: sig(:), eps(:), cmat(:,:)
       character*6,allocatable:: seq(:), A(:), B(:)
       character*6, inA, inB

   
       nlj = 355746
       chi = 0.d00

       open(unit=3, file='param.dat')
       open(unit=4, file='matrix.param.dat')

       allocate(sig(nlj))
       allocate(eps(nlj))
       allocate(A(nlj))
       allocate(B(nlj))


! Read Martini LJ parameters       
       read(3,*)
       do i = 1, nlj
          read(3,*) A(i), B(i), dm1, sig(i), eps(i)
       enddo

! Read Input Sequence of Martini Types       
       read(5,*) nseq
       allocate(seq(nseq))
       allocate(cmat(nseq,nseq))
       do i = 1, nseq
          read(5,*) seq(i)
       enddo   

       do j = 1, nseq
        do k = 1, nseq
         statA = 0
         statB = 0
         inA = seq(j)
         inB = seq(k)
         do i = 1, nlj
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

         if(inA.eq.inB)then
             cmat(j,k) = 0.0d00
         else    
           do i = 1, nlj
             if(A(i).eq.inA.and.B(i).eq.inA)then
                tmp1 = i 
             elseif(A(i).eq.inA.and.B(i).eq.inB.or. 
     $          A(i).eq.inB.and.B(i).eq.inA)then !mix
                tmp2 = i 
             elseif(A(i).eq.inB.and.B(i).eq.inB)then
                tmp3 = i 
             endif
           enddo
           e1 = -1.0*eps(tmp1)
           e2 = -1.0*eps(tmp2)
           e3 = -1.0*eps(tmp3)
           chi = 3.0d00 * ( 2*e2 - (e1+e3) )
           cmat(j,k) = chi
         endif  
        enddo 
       enddo


! Matrix Writing  
       write(4,*) '       ', (seq(i), i=1,nseq)     
       do i = 1, nseq
          write(4,*) seq(i), (cmat(i,j), j=1,nseq)
       enddo   


!40    format(1A5, f8.4)
10    end program   

