********************************************************************************
*    Файл Prn_Inv        Разработчик Андрей Васин                18.03.2012    *
********************************************************************************
*                                                                              *
*                     Печать накладной + акта на доставку                      *
*                                                                              *
********************************************************************************
PROCEDURE Prn_Inv
PARAMETERS f_code, n_doc, d_doc

PRIVATE lwr, d_list
lwr = ""
*
* f_code - код предприятия;
* n_doc  - номер документа;
* d_doc  - дата или год предприятия
*

m.d_list = Emp_Act(f_code, n_doc, d_doc)

IF .NOT. EMPTY(LEFT(m.d_list, 1))
  DO P_Inv WITH f_code, n_doc, d_doc
ENDIF

IF .NOT. EMPTY(RIGHT(m.d_list, 1))
  DO P_Act_D WITH f_code, n_doc, d_doc
ENDIF
