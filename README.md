FORCAST Imaging and Spectroscopy Software Tools
===============================================

DISCLAIMER
==========
This Unofficial Software for FORCAST imaging and grism data processing. It was created
and maintained by Luke Keller (lkeller@ithaca.edu). We don't guarnty any support
and in no way represents an official version delivered by USRA or the FORCAST team.

Installation
============
1. Get the code
	a- Go to where you want to copy the code: cd directory-to-copy-repository
	b- Create an empty directory:mkdir drip_fg && cd drip_fg
	c- Clone the repository: git clone https://github.com/lkeller/drip_fg.git
	d- Change executable rights of the script: 
	        + chmod u+x install_forcast
		+ chmod u+x config_setup
		+ chmod u+x forcast_dir_setup
2. Install the code
	a- To install it in a separate directory (new-dir/) [NOTE: Outside directory-to-copy-repository/drip]
		+ Run: ./install_forcast -s directory-to-copy-repository/drip_fg new-dir
        b- To just modify the configuration of the text files in the cloned repository
		+ Run: ./install_forcast directory-to-copy-repository/drip_fg 

NOTE: It is recommended for developers to "install" the code in the same directory
as the cloned repository since they will be able to synchronize the changes.
