      subroutine soltout
      use parm
      implicit none
      integer :: j, k, nly
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j) 
       
       do k= 1, nly1-1
       
       if (sol_nd(k+1,j)>50. .and. sol_nd(k,j)<=50.) then
       soltmp_50(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-50) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                   + sol_tmp(k+1,j) * ((50- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>100. .and. sol_nd(k,j)<=100.) then
       soltmp_100(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-100) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                  + sol_tmp(k+1,j) * ((100- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>150. .and. sol_nd(k,j)<=150.) then
       soltmp_150(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-150) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                  + sol_tmp(k+1,j) * ((150- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>200. .and. sol_nd(k,j)<=200.) then
       soltmp_200(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-200) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                  + sol_tmp(k+1,j) * ((200- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>300. .and. sol_nd(k,j)<=300.) then
       soltmp_300(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-300) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                  + sol_tmp(k+1,j) * ((300- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>500. .and. sol_nd(k,j)<=500.) then
       soltmp_500(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-500) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                   +sol_tmp(k+1,j) * ( (500- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       if (sol_nd(k+1,j)>1000. .and. sol_nd(k,j)<=1000.) then
       soltmp_1000(j)= sol_tmp(k,j) * ( (sol_nd(k+1,j)-1000) / (sol_nd(k+1,j)-sol_nd(k,j)) ) &
                  + sol_tmp(k+1,j) * ( (1000- sol_nd(k,j)) / (sol_nd(k+1,j)-sol_nd(k,j)) )
       end if
       
       end do
      
      return
      end

     