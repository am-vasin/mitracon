*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Round_P      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              ｪ珮��キ┘ ���-�� ぎ 罐���� 腮甄� 祚���〓�.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.04.1999 様�
PROCEDURE Round_P
PARAMETERS nm, q, q_pack, is_pack, st_price

PRIVATE n_p, l, mss

*IF mode_rnd = 3 .OR. is_pack <= 1   && �ウ━ �覈�鈑キ
IF is_pack <= 1   && �ウ━ �覈�鈑キ
  RETURN q
ENDIF

IF round_1   && �ウ━ �覈�鈑キ
  round_1 = .F.
  RETURN q
ENDIF

DO CASE
CASE sale_mode = "Mitracon"
  IF is_pack > 1 .AND. q # 0 .AND. mode_rnd # 3
    RETURN MAX(q, is_pack)
  ENDIF
CASE sale_mode = "Mitin"	&& �┴���� ��爼� �皴竅��!
  RETURN q
OTHERWISE
  IF is_pack > 1
    n_p = CEILING(q/is_pack)
    RETURN n_p*is_pack
  ENDIF

  n_p = CEILING(q/MAX(q_pack,1))
  IF mode_rnd = 1 .AND. n_p*MAX(q_pack,1) # q  && �竍��� 爛Θ�
    DIMENSION mss(5)
    mss(1) = ""
    mss(2) = CHR(0)+"��Ж罔� "+ALLTRIM(nm)+":"
    mss(3) = CHR(0)+"���牀茱�� "+ALLTRIM(STR(q))+", ��爼� 祚���→� - "+ALLTRIM(STR(q_pack))+","
    mss(4) = CHR(0)+"  �皴竅皋� "+ALLTRIM(STR(q_pack*n_p))+"? [ �� ] [ �モ ]"
    mss(5) = ""
    RETURN IIF(Out_Mess(5,"mss") = 1, n_p*q_pack, q)
  ENDIF

  IF st_price < mode_pri .AND. BETWEEN((n_p*q_pack-q)/q, 0, mode_part)
    RETURN n_p*q_pack
  ENDIF
ENDCASE

RETURN q
