*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл El_Ord       Разработчик Андрей Васин           02.09.97 10:55:28 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║      Формирование заявки на поставку на основании приемного акта.      ║
*║                             DBF - формат.                              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE El_Ord
PARAMETERS frm, doc_n, doc_d, d_type
*
*  frm    - предприятие;
*  doc_n  - номер приемного акта;
*  doc_d  - либо дата накладной либо год в символьном или числовом формате;
*
PRIVATE sel_rem, d_year, mss, own_c, k00, w

sel_rem = SELECT()

IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

SELECT 0
USE (base_path+"CUSTOMER")
LOCATE FOR SELF # " "
IF FOUND()
  own_c = CODE
ELSE
  own_c = 0
ENDIF

USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS ACT_TTT AGAIN
k00 = frm+d_year+doc_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет приенного акта с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
DELETE FILE (tmpo_path+"ORD_TIT.DBF")
DELETE FILE (tmpo_path+"ORDER.DBF")

SELECT 0
CREATE DBF (tmpo_path+"ORD_TIT.DBF") ;
  ( ORD_NUM  C(10), ;
    ORD_DATE D,     ;
    CUS_CODE N( 8), ;
    SUP_CODE N( 8), ;
    EXP_DOC  C(10), ;
    EXP_DATE D,     ;
    WHO_ORD  N( 3), ;
    WHERE_ORD C( 3)  )

APPEND BLANK
REPLACE ORD_NUM   WITH ACT_TTT.DOC_NUM, ;
        ORD_DATE  WITH ACT_TTT.DOC_DATE,;
        CUS_CODE  WITH own_c,           ;
        SUP_CODE  WITH ACT_TTT.SUP_CODE,;
        WHO_ORD   WITH ACT_TTT.WHO_ACT, ;
        WHERE_ORD WITH ACT_TTT.WHERE_ACT

USE
CREATE DBF (tmpo_path+"ORDER.DBF") ;
  ( ORD_NUM  C(10), ;
    ORD_DATE D,     ;
    CODE     N( 7), ;
    QNT      N( 6) )

SELECT ACT_TTT
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS ACT_TTT AGAIN
SEEK k00

SCAN REST WHILE FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = k00
  IF QNT_INC = 0
    LOOP
  ENDIF
  SELECT ORDER
  APPEND BLANK
  REPLACE ORD_NUM  WITH ACT_TTT.DOC_NUM, ;
          ORD_DATE WITH ACT_TTT.DOC_DATE,;
          CODE     WITH ACT_TTT.CODE,    ;
          QNT      WITH ACT_TTT.QNT_INC
  SELECT ACT_TTT
ENDSCAN

USE

SELECT ORDER
USE

RELEASE k00

= ADIR(k000, tmpo_path+"ORD_TIT.DBF")
own_c = k000(1,2)
= ADIR(k000, tmpo_path+"ORDER.DBF")
own_c = own_c+k000(1,2)

w = ""
DO WHILE .T.
  w = Int_Dir(w, "Куда пишем заявку?", own_c)
  IF EMPTY(w)
    EXIT
  ENDIF
  IF File_O(w+"ORDER.DBF") .OR. File_O(w+"ORD_TIT.DBF")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Там уже есть заявка: [ Заменить ] [ Пишем в другое место ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      LOOP
    ENDIF
    DELETE FILE (w+"ORD_TIT.DBF")
    DELETE FILE (w+"ORDER.DBF")
  ENDIF
  COPY FILE (tmpo_path+"ORD_TIT.DBF") TO (w+"ORD_TIT.DBF")
  COPY FILE (tmpo_path+"ORDER.DBF") TO (w+"ORDER.DBF")
  EXIT
ENDDO

DELETE FILE (tmpo_path+"ORD_TIT.DBF")
DELETE FILE (tmpo_path+"ORDER.DBF")

SELECT (sel_rem)

RETURN
