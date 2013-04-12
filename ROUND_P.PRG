*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Round_P      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�              ���㣫���� ���-�� �� 楫��� �᫠ 㯠�����.               �
*�                                                                        �
*����������������������������������������������������������� 28.04.1999 �ͼ
PROCEDURE Round_P
PARAMETERS nm, q, q_pack, is_pack, st_price

PRIVATE n_p, l, mss

*IF mode_rnd = 3 .OR. is_pack <= 1   && ����� �몫�祭
IF is_pack <= 1   && ����� �몫�祭
  RETURN q
ENDIF

IF round_1   && ����� �몫�祭
  round_1 = .F.
  RETURN q
ENDIF

DO CASE
CASE sale_mode = "Mitracon"
  IF is_pack > 1 .AND. q # 0 .AND. mode_rnd # 3
    RETURN MAX(q, is_pack)
  ENDIF
CASE sale_mode = "Mitin"	&& ������� ���� ���᪠!
  RETURN q
OTHERWISE
  IF is_pack > 1
    n_p = CEILING(q/is_pack)
    RETURN n_p*is_pack
  ENDIF

  n_p = CEILING(q/MAX(q_pack,1))
  IF mode_rnd = 1 .AND. n_p*MAX(q_pack,1) # q  && ��筮� ०��
    DIMENSION mss(5)
    mss(1) = ""
    mss(2) = CHR(0)+"������ "+ALLTRIM(nm)+":"
    mss(3) = CHR(0)+"����襭� "+ALLTRIM(STR(q))+", ��ଠ 㯠����� - "+ALLTRIM(STR(q_pack))+","
    mss(4) = CHR(0)+"  ����⨬ "+ALLTRIM(STR(q_pack*n_p))+"? [ �� ] [ ��� ]"
    mss(5) = ""
    RETURN IIF(Out_Mess(5,"mss") = 1, n_p*q_pack, q)
  ENDIF

  IF st_price < mode_pri .AND. BETWEEN((n_p*q_pack-q)/q, 0, mode_part)
    RETURN n_p*q_pack
  ENDIF
ENDCASE

RETURN q