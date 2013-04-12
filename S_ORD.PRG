*������������������������������������������������������������������������ͻ
*� ���� S_Ord        ���ࠡ��稪 ���३ ��ᨭ           19.05.98 15:51:55 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                 ������ ������ � DBF-䠩� ��� ��।��.                 �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE S_Ord
PARAMETERS frm00, ord_n, ord_d, supName
*
*  frm00 - �।���⨥;
*  ord_n - ����� ������ ( ����७��� �ଠ� );
*  ord_d - ���� ��� ������ ���� ��� � ᨬ���쭮� ��� �᫮��� �ଠ�.
*

PRIVATE sel_rem, d_year, pth, n, sz, mss

sel_rem = SELECT()

IF TYPE("ord_d") = "D"
  d_year = LEFT(DTOS(ord_d),4)
ENDIF
IF TYPE("ord_d") = "N"
  d_year = STR(ord_d,4)
ENDIF
IF TYPE("ord_d") = "C"
  d_year = ord_d
ENDIF

PRIVATE d_n, d_d, sup_n, cus_n
d_n = IIF(TYPE("ord_n") = "C", PADL(ALLTRIM(ord_n), 10), STR(ord_n,10))

SELECT 0
USE (base_path+"ORD_TIT") ORDER TAG ORD_NUM ALIAS INV_TTT AGAIN
IF .NOT. SEEK(frm00+d_year+d_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��� � ����஬ "+ALLTRIM(ord_n)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
ENDIF
sz = HEADER()+1
SELECT 0
USE (base_path+"ORDER") ORDER TAG ORD_NUM ALIAS INV_DET AGAIN
SEEK d_year+d_n
COUNT REST WHILE ORD_NUM = d_n .AND. FIRM = frm00 .AND. LEFT(DTOS(ORD_DATE),4) = d_year TO n
sz = sz+n*RECSIZE()
SEEK frm00+d_year+d_n

pth = ""
DO WHILE .T.
  pth = Int_Dir(pth, "���� ������ "+supName, sz)
  IF EMPTY(pth)
    USE
    IF USED ("INV_TTT")
      SELECT INV_TTT
      USE
    ENDIF
    SELECT(sel_rem)
    RETURN
  ENDIF
  pth = ALLTRIM(pth)
  IF File_O(pth+"ORD_S.DBF") .OR. File_O(pth+"ORD_S_T.DBF")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��� 㦥 ���� ���!, ��� ����㯨�?"
    mss(3) = CHR(0)+"   [ ����襬 ������ ] [ ����襬 � ��㣮� ���� ] [ �⪠����� ]"
    mss(4) = ""
    n = Out_Mess(7, "mss")
    IF n = 3
      USE
      SELECT INV_TTT
      USE
      SELECT(sel_rem)
      RETURN
    ENDIF
    IF n = 2
      LOOP
    ENDIF
    IF n = 1
      DELETE FILE (pth+"ORD_S.DBF")
      DELETE FILE (pth+"ORD_S_T.DBF")
    ENDIF
  ENDIF
  EXIT
ENDDO

COPY REST WHILE ORD_NUM = d_n .AND. FIRM = frm00 .AND. LEFT(DTOS(ORD_DATE),4) = d_year TO (pth+"ORD_S.DBF")
USE
SELECT INV_TTT
COPY NEXT 1 TO (pth+"ORD_S_T.DBF")
USE

SELECT (sel_rem)

RETURN