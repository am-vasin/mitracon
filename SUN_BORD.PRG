*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                         ����� � ��め▲皖��.                           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sun_Bord

PARAMETERS l0, c0, l1, c1, title

PRIVATE sun_col

m.sun_col = SCHEME(14,2)

@ m.l0, m.c0 TO m.l1, m.c1

@ m.l1, m.c0+1 FILL TO m.l1, m.c1 COLOR (m.sun_col)

@ m.l0, m.c1 FILL TO m.l1, m.c1 COLOR (m.sun_col)

IF TYPE("m.title") = "C"
  @ l0, c0+2 SAY m.title
ENDIF

RETURN
