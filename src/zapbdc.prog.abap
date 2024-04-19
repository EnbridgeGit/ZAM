report ZAPBDC no standard page heading.

include bdcrecxx.

start-of-selection.

perform open_group.

perform bdc_dynpro      using 'SAPMS01J' '0200'.
perform bdc_field       using 'BDC_OKCODE'
                              'ADDU'.
perform bdc_field       using 'BDC_CURSOR'
                              'XU200-XUSER'.
perform bdc_field       using 'XU200-XUSER'
                              'testbdc'.
perform bdc_dynpro      using 'SAPLSUSB' '0705'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'XU213-PROFILE(01)'.
perform bdc_field       using 'XU213-BCODE'
                              'welcome'.
perform bdc_field       using 'XU213-BCODE2'
                              'welcome'.
perform bdc_field       using 'XU213-PROFILE(01)'
                              's_a.user'.
perform bdc_dynpro      using 'SAPLSUSB' '0705'.
perform bdc_field       using 'BDC_OKCODE'
                              'CONT'.
perform bdc_field       using 'BDC_CURSOR'
                              'USR02-CLASS'.
perform bdc_dynpro      using 'SAPLSUSB' '0701'.
perform bdc_field       using 'BDC_OKCODE'
                              'CONT'.
perform bdc_field       using 'BDC_CURSOR'
                              'USR03-SALUT'.
perform bdc_dynpro      using 'SAPLSUSB' '0702'.
perform bdc_field       using 'BDC_OKCODE'
                              'CONT'.
perform bdc_field       using 'BDC_CURSOR'
                              'XU310-DCPFM2'.
perform bdc_field       using 'XU310-DATFM1'
                              ''.
perform bdc_field       using 'XU310-DCPFM1'
                              ''.
perform bdc_field       using 'XU310-DCPFM2'
                              'X'.
perform bdc_field       using 'XU310-DATFM3'
                              'X'.
perform bdc_dynpro      using 'SAPLSUSB' '0703'.
perform bdc_field       using 'BDC_OKCODE'
                              'CONT'.
perform bdc_field       using 'BDC_CURSOR'
                              'XU350-PARID(01)'.
perform bdc_dynpro      using 'SAPMS01J' '0200'.
perform bdc_field       using 'BDC_OKCODE'
                              'BACK'.
perform bdc_field       using 'BDC_CURSOR'
                              'XU200-XUSER'.
perform bdc_transaction using 'SU01'.

perform close_group.
