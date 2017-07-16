 Copyright c 1998-2002 The Board of Trustees of the University of Illinois

 All rights reserved.

 Developed by:	Large Scale Systems Research Laboratory

 Professor Richard Braatz, Director
   Department of Chemical Engineering
    University of Illinois
     http://brahms.scs.uiuc.edu



Permission hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the"Software"), to
 deal with the Software without restriction, including without limitation
 the rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons to whom the 
 Software is furnished to do so, subject to the following conditions:

1. Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimers.

2. Redistributions in binary form must reproduce the above 
 copyright notice, this list of conditions and the following 
disclaimers in the           documentation and/or other materials provided with the distribution.

3. Neither the names of Large Scale Research Systems Laboratory,
 University of Illinois, nor the names of its contributors may
 be used to endorse or promote products derived from this  Software without specific prior written permission.


THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 DEALINGS IN THE SOFTWARE.

	

This directory contains the Fortran 77 codes for the open-loop 
and the closed-loop simulations for the Tennessee Eastman 
process (TEP) as well as the training and testing data files used for
 evaluating the data-driven methods (PCA, PLS, FDA, and CVA).  

The 
descriptions of each file is shown below:


File name:	
Description:


temain.f		open loop simulation codes for the TEP

temain_mod.f	closed loop simulation codes for the TEP

teprob.f		subprogram for the simulation codes for the TEP

d00.dat		training file for the normal operating conditions          d00_te.dat	testing file for the normal operating conditions
d
01.dat		training file for Fault 1                                                     d01_te.dat	testing file for Fault 1 

d02.dat		training file for Fault 2 			d02_te.dat	testing file for Fault 2 
     .			d21.dat		training file for Fault 21			d21_te.dat	testing file for Fault 21

	
Each training data file contains 480 rows and 52 columns and 
each testing data file contains 960 rows and 52 columns.  An observation 
vector at a particular time instant is given by

 x=[XMEAS(1), XMEAS(2), ..., XMEAS(41), XMV(1), ..., XMV(11)]^T
 where XMEAS(n) is the n-th measured variable and 
XMV(n) is the n-th manipulated variable.


       
