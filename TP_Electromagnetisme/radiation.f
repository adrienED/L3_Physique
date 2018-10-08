c******************************************************************

c******************************************************************

! 09 janvier 2004

  
      PROGRAM LARMOR

     
!    Trajecroire d un electron - libre - plasma - acceleration longitudinale
!    Rayonnement de cet electron - distribution angulaire
!                                - spectre


c******************************************************************

c***************  PROGRAMME PRINCIPAL *****************************

c****************************************************************** 

      
      IMPLICIT NONE

      REAL*8 a0,pi,dt,xt,vacc,tobs,selec2,timpul
      COMMON a0,dt,vacc,timpul
      COMMON npt,selecenvel
      INTEGER npt,nps,selec1
      INTEGER selecenvel


      PRINT *, '*******************************************************'
      PRINT *, ' Le programme Rad permet de calculer : '      
      PRINT *, '            '      
      PRINT *, ' 1) La trajectoire d un electron soumis au champ '
      PRINT *, '    electromagnetique cree par un laser intense.'     
      PRINT *, '            '      
      PRINT *, ' 2)   L energie electromagnetique rayonnee par '
      PRINT *, '   l electron dans toutes les directions de l espace.'    
      PRINT *, '            '      
      PRINT *, ' Dans ce programme, la polarisation du champ '
      PRINT *, ' electromagnetique cree par le laser est lineaire.'
      PRINT *, ' Le champ electrique est suivant l axe Ox, et le champ'
      PRINT *, '  magnetique est suivant l axe Oy. '
      PRINT *, '              '        
      PRINT *, '*******************************************************'
      
      WRITE(*,*)'Parametre a0 = ?'
      READ(*,*) a0 

!      WRITE(*,*)'Amplitude de la force plasma = ?'
!      READ(*,*) fplasma  


      WRITE(*,*)'Avec ou sans enveloppe temporelle = (1/0)?'
      READ(*,*) selecenvel

      IF (selecenvel .eq. 1) THEN
      WRITE(*,*)'Definir le parametre de duree de l impulsion= ?'
      READ(*,*) timpul
      ELSE
	ENDIF


      WRITE(*,*)'Definir le nombre de pas de temps = ?'
      READ(*,*) npt
c      WRITE(*,*)'Pas de temps 1/x, x = ?'
c      READ(*,*) xt  
      WRITE(*,*)'Definir le nombre de pas dans le spectre = ?'
      READ(*,*) nps

      WRITE(*,*)'Calcul automatique de l angle = (1/0)?'
	READ(*,*)selec1

      IF (selec1 .eq. 0) THEN
      WRITE(*,*)'Definir l angle observation du rayonnement(1)= ?'
      READ(*,*)selec2
      ELSE
	ENDIF


!      dt=1./xt 


c PARAMETRES###############


! choix des parametres : a0 intensite laser - parametre de force
!                        npt nombre de pas de temps pour le calcul de la trajectoire
!                        l enveloppe fait 140 - lorsque la phase finale est de l ordre de 140 c est bien
!                        dt pas de temps, important pour la resolution spectrale (ex : 400 resoud la 200 eme harmonique)
!                        nps nombre de pas dans le spectre (position de la frequence maximale) 
!                        la frequence d echantillonage est de 0.02

c #########################

      pi=acos(-1.)
!      a0=1.2 !intensite laser 0.85 * sqrt (lambda(microns)**2*I/(10E18 W/cm2))

c nombre de pas de temps
!	npt=10000.
      dt=1./200.

c nombre de points dans le spectre
!      nps=1000                                

!L'acceleration en plus
	vacc=0.   

!Angle d observation
	tobs=selec2*pi/180. 


c #########################

      CALL trajectoire

      CLOSE(14)
      CLOSE(15)
      CLOSE(16)

      IF (selec1 .eq. 1) THEN
      CALL angle
      ELSE
      CALL angledef(tobs)
      ENDIF

      CLOSE(17)
      CLOSE(18)

      CALL pourTF

      CLOSE(20)
      CLOSE(21)
      CLOSE(22)  

      CALL TFparsomme(nps)      

      CLOSE(23)

      STOP
      END  


c******************************************************************
c***************  FIN DU PROGRAMME PRINCIPAL **********************
c******************************************************************


c******************************************************************
c***************  SUBROUTINES  ************************************
c****************************************************************** 


      SUBROUTINE trajectoire  
      IMPLICIT NONE
      COMMON a0,dt,vacc,timpul
      COMMON npt
      INTEGER ND
      INTEGER npt
      REAL*8 a0,dt,vacc,timpul
      PARAMETER (ND=6)
      INTEGER delta,nlarge
      REAL*8 e0,a,dtau,epsilon,dphi
      REAL*8 duree,w0,lambda0,y(ND),dydt(ND),
     *       t,gamma0,eps,h,pi,
     *       x0,y0,z0,px0,py0,pz0,tini,t0,fin
      REAL*8 statO(0:ND,0:npt),yout(npt),che(npt),
     *       chb(npt),letemps(npt),xgamma(npt)       
      INTEGER i,ipas,j,npas
      EXTERNAL derivs
      OPEN(14,file='imp.res',form='formatted')
      OPEN(15,file='traj.dat',form='formatted')
      OPEN(25,file='trajT.dat',form='formatted')

      
      px0=0.
	py0=0.
	pz0=0.
	x0=0.
	y0=0.
	z0=0.  

      pi=acos(-1.d0)
      gamma0=sqrt(1+px0**2+py0**2+pz0**2)
	WRITE(*,*)gamma0

      lambda0=0.8

! calcul d'une trajectoire


      tini=0.
      t0=0.
      y(1)=2.*pi*x0/lambda0
      y(2)=2.*pi*y0/lambda0
      y(3)=2.*pi*z0/lambda0
      y(4)=px0
      y(5)=py0
      y(6)=pz0

      dydt(1)=px0/gamma0
      dydt(2)=py0/gamma0
      dydt(3)=pz0/gamma0
      dydt(4)=0.d0
      dydt(5)=0.d0
      dydt(6)=0.d0

      t=t0
      h=dt       

      do ipas=1,npt

        call champs(y,t,che,chb)
         letemps(ipas)=t

        call rk4(y,dydt,ND,t,h,yout,derivs)
        do i=1,ND
         y(i)=yout(i)
        enddo

        statO(0,ipas)=t-y(3)
        do i=1,ND
          statO(i,ipas)=y(i)
        enddo

        xgamma(ipas)=sqrt(1.+y(4)**2+y(5)**2+y(6)**2)
        t=t+h

        call derivs(t,y,dydt)

      enddo

      do i=1,npt 
         statO(0,i)=statO(0,i)
         statO(1,i)=statO(1,i)
         statO(2,i)=statO(2,i)
         statO(3,i)=statO(3,i)
      enddo

      WRITE(*,*)'Phase finale =',t-y(3)

      do i=1,npt  
         WRITE(15,*)(statO(j,i),j=1,3)
c temps, position longitudinale, impulsion
         WRITE(14,*)statO(4,i),statO(5,i),statO(6,i)

      enddo
       do i=1,npt,100  
         WRITE(25,*)(statO(j,i),j=1,3)
      enddo


      WRITE(*,*)'Trajectoire calculee'
      return 
      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



      SUBROUTINE derivs(t,y,dydt)
      IMPLICIT NONE
      COMMON a0,dt,vacc,timpul
	COMMON npt
      REAL*8 a0,dt,vacc,timpul
      INTEGER npt,delta,nlarge
      REAL*8 e0,a,dtau,epsilon,dphi,che(3),chb(3)
      INTEGER ND
      PARAMETER (ND=6)
      REAL*8 t,y(ND),dydt(ND),gamma1,eb(4)
         call champs(y,t,che,chb)
         eb(3)=0.d0
         eb(4)=0.d0
         gamma1=1.d0/sqrt(1+y(4)**2+y(5)**2+y(6)**2)
         dydt(1)=y(4)*gamma1
         dydt(2)=y(5)*gamma1
         dydt(3)=y(6)*gamma1
         dydt(4)=(dydt(3)-1.d0)*che(1)
         dydt(5)=(dydt(3)-1.d0)*che(2)
         dydt(6)=-dydt(1)*che(1)-dydt(2)*che(2)

c ACCELERE - 256 MeV en 800 microns 
c L'energie finale est lue a partir de l'impulsion tracée : E(MeV)=0.25 * P(lue)**2
!     &   +vacc*1.E-3
c plasma case
     &   -0.*y(3)
!        else
!        endif
      return
      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



      SUBROUTINE champs(y,t,che,chb)
      IMPLICIT NONE
      COMMON a0,dt,vacc,timpul
	COMMON npt,selecenvel
      INTEGER npt,selecenvel,toto
      REAL*8 dt,a0,vacc,timpul 
      INTEGER i,ND
      PARAMETER (ND=6)
      REAL*8 y(ND),t,che(3),chb(3)
      REAL*8 phi,pi
      pi=acos(-1.)
      phi=t-y(3)


      if (selecenvel.eq.1) then

c polarisation lineaire

!       write(*,*) 'avec enveloppe'
!      read(*,*) toto
      che(1)=a0*cos(phi)*sin(phi*pi/timpul)**2
      che(2)=0.d0
      che(3)=0.d0


      chb(1)=0.d0
      chb(2)=a0*cos(phi)*sin(phi*pi/timpul)**2
      chb(3)=0.d0


      else

c polarisation lineaire sans enveloppe


      che(1)=a0*cos(phi)
      che(2)=0.d0
      che(3)=0.d0

      chb(1)=0.d0
      chb(2)=a0*cos(phi)
      chb(3)=0.d0

      endif 



c sans enveloppe

!      che(1)=a0/sqrt(2.)*sin(phi)
!      che(2)=-a0/sqrt(2.)*cos(phi)
!      che(3)=0.d0

!      chb(1)=a0/sqrt(2.)*cos(phi)
!      chb(2)=a0/sqrt(2.)*sin(phi)
!      chb(3)=0.d0

c polarisation circulaire


!      che(1)=a0/sqrt(2.)*sin(phi)*sin(phi*pi/100.)**2
!      che(2)=-a0/sqrt(2.)*cos(phi)*sin(phi*pi/100.)**2
!      che(3)=0.d0

!      chb(1)=a0/sqrt(2.)*cos(phi)*sin(phi*pi/100.)**2
!      chb(2)=a0/sqrt(2.)*sin(phi)*sin(phi*pi/100.)**2
!      chb(3)=0.d0

c sans laser



!      che(1)=0.d0
!      che(2)=0.d0
!      che(3)=0.d0
!
!      chb(1)=0.d0
!      chb(2)=0.d0
!      chb(3)=0.d0

      return

      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



      SUBROUTINE rk4(y,dydt,ND,t,h,yout,derivs)
      INTEGER ND,NMAX
      REAL*8 h,t,dydt(ND),y(ND),yout(ND)
      EXTERNAL derivs
      PARAMETER (NMAX=6)

      INTEGER i
      REAL*8 h6,hh,th,dym(NMAX),dyt(NMAX),yt(NMAX)
      hh=h*0.5
      h6=h/6.
      th=t+hh
      do i=1,ND
         yt(i)=y(i)+hh*dydt(i)
      enddo

      call derivs(th,yt,dyt)
      do i=1,ND
         yt(i)=y(i)+hh*dyt(i)
      enddo

      call derivs(th,yt,dym)
      do i=1,ND
         yt(i)=y(i)+h*dym(i)
         dym(i)=dyt(i)+dym(i)
      enddo

      call derivs(t+h,yt,dyt)
      do i=1,ND

      yout(i)=y(i)+h6*(dydt(i)+dyt(i)+2.*dym(i))

      enddo
      return
      END


****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



c Produit vectoriel

      SUBROUTINE prodvec(v1,v2,v3)

      REAL*8 v1(3),v2(3),v3(3)

      v3(1)=v1(2)*v2(3)-v1(3)*v2(2)
      v3(2)=v1(3)*v2(1)-v1(1)*v2(3)
      v3(3)=v1(1)*v2(2)-v1(2)*v2(1)

      return
      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



c Produit scalaire



      SUBROUTINE prodscal(v1,v2,v3)

      REAL*8 v1(3),v2(3)
      REAL*8 v3
      v3=v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)
      return
      end


****5***10***15***20***25***30***35***40***45***50***55***60***65***70**
****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

c Dérivée d'une fonction f

       SUBROUTINE deriv(np,dtx,f,D)

       REAL*8 f(np)
       REAL*8 D(np)
       REAL*8 dtx
       D(1)=0
       do i=2,np-1
        x=i*dtx
        D(i)=(f(i+1)-f(i))/dtx
       enddo

       return
       end

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



c Somme de vecteurs

      SUBROUTINE somvec(v1,v2,v3)

      REAL*8 v1(3),v2(3),v3(3)

      v3(1)=v1(1)+v2(1)
      v3(2)=v1(2)+v2(2)
      v3(3)=v1(3)+v2(3)

      return
      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**
****5***10***15***20***25***30***35***40***45***50***55***60***65***70**


c Différence de vecteurs

      SUBROUTINE diffvec(v1,v2,v3)

      REAL*8 v1(3),v2(3),v3(3)

      v3(1)=v1(1)-v2(1)
      v3(2)=v1(2)-v2(2)
      v3(3)=v1(3)-v2(3)

      return
      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**
****5***10***15***20***25***30***35***40***45***50***55***60***65***70**


c Norme d'un vecteur


      SUBROUTINE normvec(v,nv)

      REAL*8 v(3)
      REAL*8 nv

      nv=sqrt(v(1)**2+v(2)**2+v(3)**2)

      return
      end

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**
****5***10***15***20***25***30***35***40***45***50***55***60***65***70**


      SUBROUTINE angledef(tobs)


	REAL*8 amax(3)
      REAL*8 tobs,pi
      OPEN(18,file='n.res',form='formatted') 
      pi=acos(-1.) 
!     tobs=5.*pi/180. 
	amax(1)=sin(tobs)
	amax(2)=0.
	amax(3)=cos(tobs)

      WRITE(18,*)amax(1),amax(2),amax(3)
      END

      SUBROUTINE angle
      IMPLICIT NONE
      COMMON a0,dt,vacc,timpul
	COMMON npt
      INTEGER npt
      REAL*8 a0,dt,vacc,timpul
      REAL*8 m,c,pi,omega,gamma,Const,q,xxx
      REAL*8 ax(npt),ay(npt),az(npt),yt(npt)
      REAL*8 Beta(3),Betax(npt),Betay(npt),Betaz(npt)
      REAL*8 P(npt),amax(3)
      REAL*8 x(npt+1),y(npt+1),z(npt+1)
      REAL*8 px(npt),py(npt),pz(npt)
      REAL*8 accx(npt),accy(npt),accz(npt)
      REAL*8 temps1(npt),temps2(npt), temps3(npt)
	REAL*8 xxrt

      INTEGER i
 
c variables S.N



      REAL*8 T2(3),T3(3),T4(3),acc(3)
      REAL*8 n(3),NP,NMax,Betan,l,POSMAX
      INTEGER j,k

      q=-1.6E-19
      m=9.1E-31
      c=3.E8

      omega=2.35619E25
      pi=acos(-1.)
      Const=(2.*q**2)/(3.)

      OPEN(14,file='imp.res',form='formatted')
      OPEN(18,file='n.res',form='formatted')   
      OPEN(17,file='dist.dat',form='formatted')

c depart ... et initialisations

      x(1)=0.
      y(1)=0.
      z(1)=0.

c ouverture et lecture des fichiers nécessaires


      do i=1,npt
       READ(14,*)px(i),py(i),pz(i)
      enddo
 
      CLOSE(14) 

      do i=1,npt
        gamma=sqrt(1+px(i)**2+py(i)**2+pz(i)**2)
        Betax(i)=px(i)/gamma
        Betay(i)=py(i)/gamma
        Betaz(i)=pz(i)/gamma
      enddo


      call deriv(npt,dt,Betax,accx)
      call deriv(npt,dt,Betay,accy)
      call deriv(npt,dt,Betaz,accz)
   

c Distribution angulaire


      POSMAX=0.
      do j=0,600 ! boucle sur la direction x
         k=sqrt(600.**2-j**2)
         l=0.
         NMax=0.

         do i=1,npt,10 ! boucle sur le temps

           NP=0.
           acc(1)=accx(i)
           acc(2)=accy(i)
           acc(3)=accz(i)

           Beta(1)=Betax(i)
           Beta(2)=Betay(i)
           Beta(3)=Betaz(i)

           n(1)=j/sqrt(j**2.+k**2.+l**2.)
           n(2)=0.
           n(3)=k/sqrt(j**2.+k**2.+l**2.)

           call diffvec(n,Beta,T2)
           call prodvec(T2,acc,T3)
           call prodscal(Beta,n,Betan)
           call prodvec(n,T3,T4)

           NP=(T4(1)**2+T4(2)**2+T4(3)**2)/(1.-Betan)**5 
           NMax=NMax+NP

         enddo ! fin de la boucle sur le temps


      if (NMax .gt. POSMAX) then

        POSMAX=NMax
        amax(1)=n(1)
        amax(2)=n(2)
        amax(3)=n(3)

      else
      endif

      xxrt=atan(n(1)/n(3))*180./pi
      WRITE(17,*)xxrt,NMax
      enddo ! fin de la boucle sur la direction x

      WRITE(18,*)amax(1),amax(2),amax(3)
      WRITE(*,*)'Angle emission maxi =',atan(amax(1)/amax(3))*180./pi
      WRITE(*,*)'Angle calcule'

      end



****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



      SUBROUTINE pourTF

      IMPLICIT NONE

      COMMON a0,dt,vacc,timpul
	COMMON npt
      INTEGER npt
      REAL*8 a0,dt,vacc,timpul
      REAL*8 m,c,pi,omega,Const,q,xxx,teta,nr
      REAL*8 ax(npt),ay(npt),az(npt),yt(npt)
      REAL*8 Beta(3),Betax(npt),Betay(npt),Betaz(npt)
      REAL*8 P(npt),letemps(npt),r(3)
      REAL*8 x(npt),y(npt),z(npt)
      REAL*8 px(npt),py(npt),pz(npt),gamma(npt)
      REAL*8 accx(npt),accy(npt),accz(npt)
      REAL*8 Poyx,Poyy,Poyz
      INTEGER i

c variables S.N

      REAL*8 T2(3),T3(3),T4(3),acc(3)
      REAL*8 n(3)
      REAL*8 NP,NMax,Betan

      q=-1.6E-19
      m=9.1E-31
      c=3.E8
      omega=2.35619E15
      pi=acos(-1.)
      Const=(2.*q**2)/(3.)
      NMax=0.

      OPEN(14,file='imp.res',form='formatted')
      OPEN(15,file='traj.dat',form='formatted')
      OPEN(18,file='n.res',form='formatted')
      OPEN(20,file='poyntingX.res',form='formatted')
      OPEN(21,file='poyntingY.res',form='formatted')
      OPEN(22,file='poyntingZ.res',form='formatted')

   
c depart ... et initialisations

      x(1)=0.
      y(1)=0.
      z(1)=0.

c ouverture et lecture des fichiers nécessaires


      do i=1,npt
        READ(14,*)px(i),py(i),pz(i)
        READ(15,*)x(i),y(i),z(i)
      enddo


      CLOSE(14)
      CLOSE(15)

      do i=1,npt
        gamma(i)=sqrt(1+px(i)**2+py(i)**2+pz(i)**2)
        Betax(i)=px(i)/gamma(i)
        Betay(i)=py(i)/gamma(i)
        Betaz(i)=pz(i)/gamma(i)
      enddo


      READ(18,*)n(1),n(2),n(3)
      CLOSE(18)

      do i=1,npt,1 ! boucle sur le temps

        NP=0.
        Beta(1)=Betax(i)
        Beta(2)=Betay(i)
        Beta(3)=Betaz(i)

        r(1)=x(i)
        r(2)=y(i)
        r(3)=z(i)

        call prodvec(n,Beta,T2)
        call prodvec(n,T2,T4)

        Poyx=T4(1)
        Poyy=T4(2)
        Poyz=T4(3)

        WRITE(20,*)Poyx
        WRITE(21,*)Poyy
        WRITE(22,*)Poyz

      enddo ! fin de la boucle sur le temps


      WRITE(*,*)'Calcul preliminaire pour TF passe'

      return
      end


****5***10***15***20***25***30***35***40***45***50***55***60***65***70**

****5***10***15***20***25***30***35***40***45***50***55***60***65***70**



      SUBROUTINE TFparsomme(nps)

      IMPLICIT NONE

      COMMON a0,dt,vacc,timpul
      COMMON npt
      INTEGER npt,nps
      REAL*8 a0,dt,vacc,timpul
      REAL*8 poyx(npt),poyy(npt),poyz(npt)
      REAL*8 omega,omegaO,t,c,pi,test,Const
      REAL*8 x(npt),y(npt),z(npt),r(3)
      REAL*8 n(3),nr
      COMPLEX ii
      COMPLEX dEx,dEy,dEz
      REAL*8 dEtotal
      INTEGER i,j

! Déclaration des constantes

      omegaO=1.
      pi=acos(-1.)
      c=1.
      ii=(0,1)
      Const=1E-30

      OPEN(15,file='traj.dat',form='formatted') 
      OPEN(18,file='n.res',form='formatted')
      OPEN(20,file='poyntingX.res',form='formatted')
      OPEN(21,file='poyntingY.res',form='formatted')
      OPEN(22,file='poyntingZ.res',form='formatted')
      OPEN(23,file='spectre60.dat',form='formatted')


! Lecture des datas dont on va faire la somme


      do i=1,npt

       READ(20,*)poyx(i)
       READ(21,*)poyy(i)
       READ(22,*)poyz(i)
       READ(15,*)x(i),y(i),z(i)
      enddo

      READ(18,*)n(1),n(2),n(3)

! début des boucles pour l'intégration

      omega=1.

      do i=1,nps ! Boucle en fréquences

      
       WRITE(*,*)nps-i  
       dEx=(0.,0.)
       dEy=(0.,0.)
       dEz=(0.,0.)
       dEtotal=0.

       omega=(i)*0.02
       do j=1,npt ! Boucle en temps
         t=j*dt   ! temps en secondes
         r(1)=x(j)*c/omegaO  !r en mètre
         r(2)=y(j)*c/omegaO
         r(3)=z(j)*c/omegaO

         call PRODSCAL(n,r,nr)

         dEx=dEx+1.*poyx(j)*exp(ii*omega*(t-nr/c))*dt
         dEy=dEy+1.*poyy(j)*exp(ii*omega*(t-nr/c))*dt
         dEz=dEz+1.*poyz(j)*exp(ii*omega*(t-nr/c))*dt

        enddo ! Fin de la boucle en temps

       dEtotal=REAL(dEx)**2+AIMAG(dEx)**2
     &        +REAL(dEy)**2+AIMAG(dEy)**2
     &        +REAL(dEz)**2+AIMAG(dEz)**2


       WRITE(23,*)omega,dEtotal*(omega)**2

      enddo ! Fin de la boucle en fréquences

      WRITE(*,*)'Frequence maxi =',nps*0.2
      WRITE(*,*)'TF passee'

      return
      end
