*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 ���������� �㬬� � 䫠���� ᯨ᪠.                    �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Refr_Lst
PARAMETERS l_num

PRIVATE strang, due_sw, dis_sw, spec_sw, def_sw, sum_wk, sel, w, def_w
strang  = .F.
due_sw  = .F.
dis_sw  = .F.
spec_sw = .F.
def_sw  = .F.
sum_wk  = 0

sel = SELECT()
SELECT 0
USE STOCK ORDER TAG CODE ALIAS S000 AGAIN
SELECT 0
USE LIST_DET ORDER TAG L_NAME ALIAS L000 AGAIN
SEEK STR(l_num,6)
SCAN REST WHILE LIST_NUM = STR(l_num,6)
  IF CODE < 0
    LOOP
  ENDIF
  IF QNT=0 .AND. QNT_DUE=0
    LOOP
  ENDIF
  IF CODE = 0
    strang = .T.      && ���� �㦠�
    def_w  = QNT > 0  && ��� � ����������� - ����� �����!
  ELSE
    IF SEEK(CODE,"S000")
      def_w = QNT-QNT_REQ
      IF def_w <= 0
        def_w = .F.   && �������� �� ����� ��� - �� �����
      ELSE
        IF def_w-S000.QNT > 0
          def_w = .NOT. EMPTY(IS_DEF)  && �� 墠⠥�, � 䫠���?
        ELSE
          def_w = .F.  && �����筮 - �� �����.
        ENDIF
      ENDIF
    ELSE
      def_w = .T.   && ��� ⠪�� ����樨, �⠫� ���� �����.
    ENDIF
  ENDIF
  def_sw = def_sw .OR. def_w
  IF QNT_DUE > 0
    due_sw = .T.    && ���� ����
  ENDIF
  IF IIF(LIST_TIT.IS_USD, USD_DEF # USD_PRICE, DEF_PRICE # SAL_PRICE)
    IF EMPTY(IS_SPEC)
      dis_sw = .T.     && ������
    ELSE
      spec_sw = .T.    && ����業�
    ENDIF
  ENDIF
  sum_wk = sum_wk+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
ENDSCAN
USE
SELECT S000
USE LIST_TIT ORDER TAG LST_NUM ALIAS L000 AGAIN
IF SEEK(l_num)
  REPLACE R_TOTAL WITH sum_wk,   ;
          IS_DEF  WITH IIF(def_sw, "*", " "),  ;
          IS_DUE  WITH IIF(due_sw, "*", " "),  ;
          IS_DIS  WITH IIF(dis_sw, "*", " "),  ;
          IS_SPEC WITH IIF(spec_sw,"*", " ")
ENDIF
USE
SELECT (sel)

RETURN strang