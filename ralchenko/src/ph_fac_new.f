      PROGRAM PHOTOFIT
      
      DOUBLE PRECISION X(5),FVEC(20),TOL,WA(1000),EX(100),
     &                 E(100),CS(100),P(5),HARVEST
      INTEGER N,M,LWA,INFO,IWA(5),ISTAT,INDEX,COUNT,MODE,SPCH
      EXTERNAL FCN_PHOTO
      EXTERNAL LMDIF1
      CHARACTER*11 FILENAME
      CHARACTER*12 INF
      COMMON /BLOCKA/ EX,CS
      COMMON /INDTOT/INDTOT
C=======Constant parameters====================================      
      N=4
      LWA =1000
C==============Default values==================================
      INF="info_ph.dat"
C=============Selecting mode===================================
c      PRINT *," Choose mode:"
c      PRINT *," 1 - Automatic read from directory"
c      PRINT *," 2 - Read from single file"
c      PRINT *," 3 - Read from terminal"
c      READ (5, FMT=*) MODE

      CALL RANDOM_SEED()

      MODE = 1

      PRINT *," Spectroscopic charge:"
      READ (5, FMT=*) SPCH

      TOL = 1e-20


222    FORMAT(3(1PE10.3,2X))
      
      IF( MODE .EQ. 1) THEN
C==============Automatic mode==================================
C==============================================================
          OPEN (10,FILE='rrec')
          ISTAT=0
          COUNT=0
          
          OPEN (12,file="info_ph_bad.dat")
          
          OPEN (13,file="output_ph.dat")

          OPEN (14,file="output_ph_bad.dat")
C================Global Loop===================================
			print *,'starting loop...'
          DO 
             COUNT=COUNT+1
C-----------------Read from data.dat---------------------------
             READ (10,*,END=111) NLOW,NUPP
             
             print*,NLOW,NUPP
             
             INDTOT=0
             CS0=0
             DO J=1,100
                READ (10,222,ERR=21) EX(J),CS(J),E(J)
c                print*,EX(J),CS(J),E(J)
                INDTOT=J
                IF (CS(J).GT.CS0) CS0=CS(J)
             ENDDO
 21          CLOSE(11)
             M=INDTOT
             DO 30 J=1,INDTOT
                CS(J)=CS(J)/CS0
 30          CONTINUE
C--------------------------------------------------------------
C----------------------Initial guess---------------------------
               X(1)=4.
               X(2)=1.
               X(3)=0.1
                   CALL DO_RAND(HARVEST)
               X(4)=HARVEST
C--------------------------------------------------------------
                 CALL LMDIF1(FCN_PHOTO,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)
                 P(1)=X(1)**2*CS0
                 P(2)=X(2)*CS0
                 P(3)=(X(3)**2-X(1)**2-X(2))*CS0
                 P(4)=X(4)
               IF (INFO.NE.1.AND.INFO.NE.2) PRINT *,"INFO=",INFO
C=================Output of the results========================
               SIGMA=0.
               DO 40 J=1,INDTOT
                 SIGMA=SIGMA+MAX(ABS(FVEC(J)),ABS(FVEC(J)/
     &                  (1.-ABS(FVEC(J)))))
  40           CONTINUE
  				IF (SIGMA.GT.1) THEN
	  				PRINT *,NLOW,'->',NUPP," Sigma = ",SIGMA
	  			ENDIF
             IF(SIGMA.GE.INDTOT) THEN
                WRITE(12,FMT=*) NLOW,'->',NUPP,SIGMA
                PRINT *," Sigma = ",SIGMA
c                WRITE(14,FMT=*)"0 0 0 0 "
                WRITE (13,96) SPCH,NLOW,NUPP,
     &          P(1),P(2),P(3),P(4),0.,0.,0.,0.,0.,0.
             ELSE  
c                WRITE(13,FMT=*)"0 0 0 0 "
                WRITE (13,96) SPCH,NLOW,NUPP,
     &          P(1),P(2),P(3),P(4),0.,0.,0.,0.,0.,0.
             END IF

C--------------------------------------------------------------
          END DO
  111     CONTINUE
 56       CONTINUE
          CLOSE(12)
          CLOSE(10)
          CLOSE(13)
          CLOSE(14)
C================End of Global Loop============================
          PRINT *,"Loops made:",COUNT
C=============Input from single file===========================
C==============================================================
          ELSE   IF(MODE .EQ. 2) THEN
          
          
             PRINT *,"Enter the name of file:"
             READ (5, *)FILENAME             
             OPEN (11,FILE=FILENAME)
             
             READ (11,*)
             READ (11,*)
C---------------Read from data file----------------------------
             INDTOT=0
             CS0=0
             DO J=1,100
               READ(11,FMT=99,END=39) EX(J),CS(J),E(J)
               INDTOT=J
               IF (CS(J).GT.CS0) CS0=CS(J)
             ENDDO
 39          CLOSE(11)
             M=INDTOT
             DO 31 J=1,INDTOT
                CS(J)=CS(J)/CS0
 31          CONTINUE
C-----------------Initial guess--------------------------------
             X(1)=1.0
             X(2)=1.0
             X(3)=1.0
             X(4)=1.0
C--------------------------------------------------------------
                 CALL LMDIF1(FCN_PHOTO,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)
                 P(1)=X(1)**2*CS0
                 P(2)=X(2)*CS0
                 P(3)=(X(3)**2-X(1)**2-X(2))*CS0
                 P(4)=X(4)
               IF (INFO.NE.1.AND.INFO.NE.2) PRINT *,"INFO=",INFO
C========================Printout==============================
             SIGMA=0
             DO 41 J=1,INDTOT
               SIGMA=SIGMA+ABS(FVEC(J))
               PRINT *,FVEC(J)
  41         CONTINUE
                PRINT *," Sigma = ",SIGMA
                PRINT *,"0 0 0 0 "
                PRINT 96, NLOW,NUPP,
     &          P(1),P(2),P(3),P(4),0,0,0,0,0,0
C--------------------------------------------------------------
          ELSE
C=======================Input from keyboard====================
C==============================================================
             PRINT *,"Enter the X points :"
             DO 22 J=1,11
                READ (5,FMT=*) EX(J)
 22          CONTINUE
             PRINT *,"Enter the Y points :"
             DO 23 J=1,11
                READ (5,FMT=*) CS(J)
 23          CONTINUE
             CS0=MAX(CS(1),CS(2),CS(3),CS(4),CS(5),CS(6),
     &           CS(7),CS(8),CS(9),CS(10),CS(11))
             DO 32 J=1,11
                CS(J)=CS(J)/CS0
 32          CONTINUE
C==============================================================
C --------------  Initial guess  ------------------------------
             X(1)=1.
             X(2)=1.
             X(3)=1.
             X(4)=1.
C--------------------------------------------------------------
             CALL LMDIF1(FCN_PHOTO,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)
             P(1)=X(1)**2*CS0
             P(2)=X(2)*CS0
             P(3)=(X(3)**2-X(1)**2-X(2))*CS0
             P(4)=X(4)
C--------------------------------------------------------------
             IF (INFO.NE.1.AND.INFO.NE.2) PRINT *,"INFO=",INFO

             PRINT *,"  ",FILENAME
             PRINT *,P(1),P(2),P(3),P(4)
             SIGMA=0
             DO 42 J=1,M
               SIGMA=SIGMA+ABS(FVEC(J))
  42         CONTINUE
             PRINT *," Sigma = ",SIGMA
      END IF
          
 100  FORMAT(A11)
 99   FORMAT(F10.2,E12.3,F13.2)
 97   FORMAT(A12)
 96   FORMAT(" ",I2," ",I6," ",I6,"  4",10(1P,E11.3,1X))
 95   FORMAT(A8)
 94   FORMAT("inf",I2.1,".dat")
 93   FORMAT("outp",I2.1,".dat")
 92   FORMAT(A11,"  ",F8.3)
      CLOSE(12)
      CLOSE(13)
      STOP
      END

      SUBROUTINE FCN_PHOTO(M,N,X,FVEC,IFLAG )
      DOUBLE PRECISION EX(100),CS(100),X(4),FVEC(20)
      COMMON /BLOCKA/ EX,CS
      COMMON /INDTOT/INDTOT
      DO 10 J=1,INDTOT
      
      FVEC(J)=((X(1)**2 + X(2)/EX(J) +
     & (X(3)**2-X(2)-X(1)**2)/EX(J)**2)/
     &        EX(J)**(3.5+X(4))-CS(J))/CS(J)
  10  CONTINUE
C      print *,FVEC(1), FVEC(10)
      RETURN
      END

c===============================================

      SUBROUTINE DO_RAND(HARVEST)
      INTEGER time(8),seed(2)        
      REAL*8 HARVEST
      
      call DATE_AND_TIME(values=time)
      seed(1) = time(4) * 
     *       (360000*time(5) + 6000*time(6) + 100*time(7) + time(8))
      
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(HARVEST)
      
      RETURN
      END
      

