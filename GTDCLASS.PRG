**********************************
PROCEDURE GTDCLASS		&&&Поиск ГТД по классам
**********************************
PARAMETERS s_code,s_num,s_date,s_country,s_countryC,s_line,s_qnt
PRIVATE tmpFile,st_code,tmGtd,tmClass,tmFlag, tmSel
st_code = s_code
tmGtd = ""
tmClass = ""

tmSel = SELECT()
*!*	B_Compel = Base_Compel+"\"
*!*	B_Store = Base_STORE +"\"
	B_Compel = Path_comm
	B_Store = Base_path
	
IF NOT USED("GT")
USE (B_Compel+"GTD_TIT") IN 0 AGAIN ALIAS "GT"
ENDIF
SET ORDER TO TAG HRON IN GT

IF NOT USED("GT_C")
USE (B_Compel+"GTD_C") IN 0 AGAIN ALIAS "GT_C"
ENDIF
SET ORDER TO TAG CLASS_USE IN GT_C

IF NOT USED("GT_SUBJ")
USE (B_Compel+"SUBJECT") IN 0 AGAIN ALIAS "GT_SUBJ"
ENDIF
SET ORDER TO TAG STO_CODE IN GT_SUBJ

IF NOT USED("GT_St")
USE (B_Store+"Stock") IN 0 AGAIN ALIAS "GT_St"
ENDIF
SET ORDER TO TAG CODE IN GT_St 


IF SEEK(st_code,"GT_SUBJ")
  IF NOT EMPTY(GT_SUBJ.CLASS_CODE)
	tmClass = GT_SUBJ.CLASS_CODE	
  ENDIF	
ENDIF
***Поищем на складе, может там есть?
IF EMPTY(tmClass)
 IF SEEK(st_code,"GT_St")
 tmClass = GT_St.CLASS
 ENDIF
ENDIF
tmFlag = .T.

	IF NOT EMPTY(tmClass)		&&&Наконец можем искать ГТД
	tmClass = ALLTRIM(tmClass)
	DO WHILE tmFlag = .T.
	  ***1Шаг.
	  SET ORDER TO TAG CLASS_USE IN GT_C
	  IF SEEK(ALLTRIM(tmClass),"GT_C")
	 	SET ORDER TO TAG CLASS IN GT_C DESCENDING	 
	 	SELECT GT_C
	 	REPLACE LAST_USE WITH ""
	 	SKIP 1
	 	 IF  CLASS = tmClass
	 	 tmGtd 	= GT_C.GTD_NUM
	 	 s_num 	= GT_C.GTD_NUM
	 	 s_date	= GT_C.GTD_DATE
	 	 s_country=GT_C.COUNTRY
	 	 s_countryC=GT_C.COUNTRYC
	 	 s_line = GT_C.GTD_LINE
	 	 s_qnt 	= GT_C.QNT
	 	 REPLACE LAST_USE WITH "*"
	 	 ENDIF
	  ENDIF
	  
	  ***2Шаг.
	  IF EMPTY(tmGtd)	&&&Может быть нет еще пока ни одной пометки или она стояла на последней записи из круга обхода.
	  SET ORDER TO TAG CLASS IN GT_C DESCENDING	
	  	IF SEEK(ALLTRIM(tmClass),"GT_C")
	  	tmGtd 	= GT_C.GTD_NUM
	 	s_num 	= GT_C.GTD_NUM
	 	s_date	= GT_C.GTD_DATE
	 	s_country=GT_C.COUNTRY
	 	s_countryC=GT_C.COUNTRYC
	 	s_line 	= GT_C.GTD_LINE
	 	s_qnt 	= GT_C.QNT

	  	REPLACE GT_C.LAST_USE WITH "*"
	  	ENDIF
	  ENDIF
	  ***3Шаг.Считаю, что не нашла по полному классу.Начинаю его обрезать
	  IF  EMPTY(tmGtd)
	  tmClass = LEFT(tmClass,LEN(tmClass)-2)
	   IF EMPTY(tmClass)
	   tmFlag = .F.
	   ENDIF
	  ELSE
	  tmFlag = .F.
	  ENDIF
	 ENDDO 
	ENDIF

IF USED("GT")
USE IN GT
ENDIF
IF USED("GT_C")
USE IN GT_C
ENDIF
IF USED("GT_SB")
USE IN GT_SB
ENDIF
IF USED("GT_ST")
USE IN GT_ST
ENDIF
IF USED("GT_SUBJ")
USE IN GT_SUBJ
ENDIF

SELECT (tmSel)
RETURN
