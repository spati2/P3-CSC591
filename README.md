P3-CSC591 (Twitter stream analysis)

*******************************************************************
* Authors: 
* Joseph Decker - jdecker
* Sadhana Kannan - sadhan
* Sattwik Pati - spati2
* Shilpa Mairpady - smairpa
* Prathyusha Vadlamudi - pvadlam

* Description :
* Twitter tweets "love/hate" sentiment classifier in real-time using HoeffdingTree classifier, continuous model update and static features


*******************************************************************
PACKAGES REQUIRED: These have been required as below in script file

*   install.packages("streamR")
*   install.packages("ROAuth")
*   install.packages("plyr")
*   install.packages("stringr")
*   install.packages("tm")
*   install.packages("SnowballC")
*   install.packages("devtools")
*   install.packages("RMOA")
*   install.packages("ff")
*   install.packages("rJava")
  
  require(streamR)
  require(ROAuth)
  require(plyr)
  require(stringr)
  require(tm)
  require(SnowballC)
  require(devtools)
  require(RMOA)
  require(ff)
  require(rJava)
  
*******************************************************************
To run script

Copy file "my_oauth.Rdata" to home or default directory of RStudio
and run project3_v3.R after selecting entire file contents to run

In case credentials are non-transferable , create variables as follows -
consumerKey <- "<replace with your twitter consumer key>"
consumerSecret <- "<replace with your twitter consumer secret key>"
and run project3_v3.R after selecting entire file contents to run


*******************************************************************
* References: 

* http://cran.r-project.org/web/packages/RMOA/RMOA.pdf
* http://pablobarbera.com/blog/archives/1.html
* https://github.com/jwijffels/RMOA
* http://cran.r-project.org/web/packages/tm/tm.pdf
* Notes by Dr. Nagiza Samatova
* http://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/
*******************************************************************