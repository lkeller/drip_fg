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

   COMMENT: It will ask you to input some paths where the data will be. It is not critical to have them right from the beginning. You will be able to set them
            when running drip later.
              * Loaddatapath contains the raw data
              * savepath: is the directory that will contain your products
              * savecalpath: is the directory that will contain the generated calibration products (master flat,...)
    

NOTE: It is recommended for developers to "install" the code in the same directory
as the cloned repository since they will be able to synchronize the changes.

How to run drip in the pipeline server?
=======================================

Manually in the server:
  - ssh -X usename@devdepot
  - ssh -X devplserv
  - if user <> dps set: "alias gdrip='/home/dps/pipelines/forcast/drip_fg/work/drip/gdrip"
  - Run gdrip:
        + gdrip: runs a session with temporary dripconf and guiconf files. They will be removed when exiting idl.
        + gdrip -u: runs a session associated to the user. It creates a dripcongusername.txt and guiconfusername.txt files that will be used next time thi
s seame user runs "gdrip -u"
        + gdrip -d: runs a session with the original dripcong and guiconf files. This is the best way to step in each others reduction. I would avoid this
 option.

*Note*: the drip can be run from anywhere in the filesystem if the alias is set correctly. One advantages is that you can set the raw, product and calibra
tion paths easily by calling it from the directory where you have your data with the following options
        + gdrip -r: Update the path in guiconf indicating the location of the raw data to the directory where you opened drip
        + gdrip -p: Update the path in guiconf indicating the location of the products to the directory where you opened drip
        + gdrip -c: Update the path in guiconf indicating the location of the produced calibration files to the directory where you opened drip
Of course, you can combine these options with each other as well as with -u or -d.

Automatic reduction in the server:
    - ssh dps@devdepot
    - ssh devplserv
    - Set IDL_STARTUP: export IDL_STARTUP=/home/dps/pipelines/forcast/drip_fg/work/drip_startup
    - cd staging_directory
    - Run drip_pipe in idl: echo "drip_pipe, 'inputset.lst', outDMfile='outputset.lst'" | idl
        Note: there is also an option for setting the path where the data is:
                    echo "drip_pipe in idl: echo "drip_pipe, 'inputset.lst', outDMfile='outputset.lst', path='the-data-path' " | idl
    - Another way to run it is to call "gdrip -acpr" from the directory containing the data.
*Note*: the directory should contain the input manifest with the list of file names



