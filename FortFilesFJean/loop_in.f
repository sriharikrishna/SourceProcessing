C ========== begin copyright notice ==============
C This file is part of 
C ---------------
C xaifBooster
C ---------------
C Distributed under the BSD license as follows:
C Copyright (c) 2005, The University of Chicago
C All rights reserved.
C
C Redistribution and use in source and binary forms, 
C with or without modification, are permitted provided that the following conditions are met:
C
C    - Redistributions of source code must retain the above copyright notice, 
C      this list of conditions and the following disclaimer.
C    - Redistributions in binary form must reproduce the above copyright notice, 
C      this list of conditions and the following disclaimer in the documentation 
C      and/or other materials provided with the distribution.
C    - Neither the name of The University of Chicago nor the names of its contributors 
C      may be used to endorse or promote products derived from this software without 
C      specific prior written permission.
C
C THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
C EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
C OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
C SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
C INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
C PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
C INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
C LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
C OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C 
C General Information:
C xaifBooster is intended for the transformation of 
C numerical programs represented as xml files according 
C to the XAIF schema. It is part of the OpenAD framework. 
C The main application is automatic 
C differentiation, i.e. the generation of code for 
C the computation of derivatives. 
C The following people are the principal authors of the 
C current version: 
C 	Uwe Naumann
C	Jean Utke
C Additional contributors are: 
C	Andrew Lyons
C	Peter Fine
C
C For more details about xaifBooster and its use in OpenAD please visit:
C   http://www.mcs.anl.gov/openad
C
C This work is partially supported by:
C 	NSF-ITR grant OCE-0205590
C ========== end copyright notice ==============
      module all_globals_mod

      end module

c$openad XXX Template ad_template.f
        subroutine foo(x,y,a,j)
          double precision, dimension(1), intent(in) :: x
          double precision, dimension(1), intent(out) :: y
	  integer a(2,2)
          integer i,j
          y(1)=x(1)
c$openad xxx simple loop
          do i=1,2
            if (a(i,j).ne.0) then
              y(1)=y(1)*x(1)*a(i,j)
            else
              y(1)=y(1)-x(1)
            end if
          end do
c$openad DEPENDENT(y)
        end subroutine


c$openad XXX Template ad_template.f
	subroutine head(x,y)
	  double precision, dimension(1), intent(in) :: x
	  double precision, dimension(1), intent(out) :: y
          integer a(2,2)
c$openad INDEPENDENT(x)
c set a
	  do i=1,2
            do j=1,2
	      a(i,j)=i+j
	    end do
	  end do
	  call foo(x,y,a,2)
c unset a to make sure it is restored
          do i=1,2
            do j=1,2
              a(i,j)=0
            end do
          end do
c$openad DEPENDENT(y)
	end subroutine
