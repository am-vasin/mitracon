*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла P_Money      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Печать ВСЕХ денежных документов.                    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 25.01.1999 ══╝
PROCEDURE P_Money
PARAMETERS firm_c, doc_t, doc_n, doc_d  &&, flgs
*
*  firm_c - код предприятия;
*  doc_t  - тип документа;
*  doc_n  - номер документа;
*  doc_d  - либо дата документа, либо год в символьном или числовом формате;
*  flg    - флажки.
*

PRIVATE s_sav   && Номер рабочей области для возврата
PRIVATE d_year  && Год документа
PRIVATE d_n     && Номер документа в текстовом виде
PRIVATE d_d     && Дата документа в текстовом виде
PRIVATE a_code  && Koд работника, оприходовавшего деньги 
PRIVATE c_name  && Имя клиента (если таковой имеет место)
PRIVATE c_code  && Код клиента (если таковой имеет место)
PRIVATE sb      && Массив описания псевдополей
PRIVATE t_d     && Массив со строками шаблона "заголовок докумета"
PRIVATE t_p     && Массив со строками шаблона "заголовок страницы"
PRIVATE f_d     && Массив со строками шаблона "подвал докумета"
PRIVATE f_p     && Массив со строками шаблона "подвал страницы"
PRIVATE n, j, i && Счетчики, индексы, параметры цикла
PRIVATE p_ln    && Длина страницы в строках
PRIVATE fnt     && Шрифт: " " - обычный, "1" - сжатый (condenced)
PRIVATE n_cp    && Число копий
PRIVATE lft     && Поля слева в символах
PRIVATE ffeed   && Завершение документа: не отрицательное - прогон строк,
                &&      отрицательное - прогон формата
PRIVATE p_drcty && Направление печати (принтер)
PRIVATE k00     && Ключ для поиска документа
PRIVATE div_arr && Для разбиения на строки
PRIVATE tmp_val
PRIVATE psp_no, psp_sr, psp_l1, psp_l2, psp_l3, face_nam
PRIVATE tmpDocType, tmpDocNum

s_sav = SELECT()

*
* Формируем год документа
IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

*
*  Заголовки документов...
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+doc_t+d_year+doc_n
*
*  Ищем...
IF .NOT. SEEK(k00)  && Не нашли...
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет документа с номером "+LEFT(doc_n,1)+ALLTRIM(RIGHT(doc_n,9))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
face_nam = CONTACT
psp_no  = PSP_SER
psp_sr  = PSP_NUM
psp_l1  = PSP_WH1
psp_l2  = PSP_WH2
psp_l3  = PSP_WH3
tmpDocType = DOC_TYPE
tmpDocNum  = DOC_NUM
IF LEFT(ENTER_DOC, 1) = CHR(0)
  tmpDocType = " "
  tmpDocNum  = SUBSTR(ENTER_DOC, 2, 10)
ENDIF
dep_nam = CUS_NAME
IF (EMPTY(face_nam) .OR. EMPTY(psp_no)) .AND. INLIST(tmpDocType, " ", "2");
                    .AND. MONEY_R < 0
  DO G_Contact
ENDIF
*
* Символьный номер документа
*IF tmpDocType = " " .AND. "0" $ firm_c
*  d_n = VAL(ALLTRIM(SUBSTR(doc_n,2)))
*  d_n = LEFT(doc_n,1)+ALLTRIM(STR(IIF(d_n > 500, 200+(d_n-200)%300, d_n)))
*ELSE
  d_n = LEFT(tmpDocNum,1)+ALLTRIM(RIGHT(tmpDocNum,6))
*ENDIF
d_d = DTOC(DOC_DATE)
a_code = WHO

*
*  Ищем клиента...
c_name = CUS_NAME 
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT

*
*  Описываем псевдополя
DIMENSION sb(50,2)

sb( 1,1) = "{N Док-та }"
sb( 1,2) = PADR(d_n, LEN(sb( 1,1)))

sb( 2,1) = "{Дата    }"
sb( 2,2) = d_d

tmp_val = ""
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_9125 AGAIN
sb(48, 1) = "{Руководитель          }"
sb(48, 2) = ""
sb(49, 1) = "{Гл. бухгалтер         }"
sb(49, 2) = ""
sb(50, 1) = "{Кассир                }"
sb(50, 2) = ""
IF SEEK(firm_c)
  tmp_val = L_NAME
  sb(48, 2) = BOSS
  sb(49, 2) = ABAK
  sb(50, 2) = KASSIR
ENDIF

sb( 3,1) = "{Предприятие                                   }"
sb( 3,2) = PADR(ALLTRIM(tmp_val), LEN(sb(3,1)))

sb( 4,1) = "{Предприятие строка 1     }"
sb( 5,1) = "{Предприятие строка 2     }"

DIMENSION div_arr(2)
div_arr(1) = LEN(sb(4,1))
div_arr(2) = LEN(sb(5,1))
DO Div_Line
sb( 4,2) = div_arr(1)
sb( 5,2) = div_arr(2)

sb( 6,1) = "{Адрес предприятия                               }"
sb( 6,2) = ADDRESS

sb( 7,1) = "{Телефон   }"
sb( 7,2) = PHONE

sb( 8,1) = "{ИНН     }"
sb( 8,2) = INN

tmp_val = ""
USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_9125 AGAIN
IF SEEK(firm_c+TIT_PRINT.BANK)
  tmp_val = L_NAME
ENDIF
sb( 9,1) = "{Банк                                            }"
sb( 9,2) = PADR(ALLTRIM(tmp_val), LEN(sb(3,1)))

sb(10,1) = "{Банк строка 1            }"
sb(11,1) = "{Банк строка 2            }"

DIMENSION div_arr(2)
div_arr(1) = LEN(sb(10,1))
div_arr(2) = LEN(sb(11,1))
DO Div_Line
sb(10,2) = div_arr(1)
sb(11,2) = div_arr(2)

sb(12,1) = "{Расчетный счет    }"
sb(12,2) = PADR(ACC_NO, LEN(sb(12,1)))

sb(13,1) = "{Корр. счет        }"
sb(13,2) = PADR(CORR_NO, LEN(sb(13,1)))

sb(14,1) = "{БИК    }"
sb(14,2) = PADR(BIK, LEN(sb(14,1)))

USE

sb(15,1) = "{Клиент                                          }"
sb(15,2) = PADR(c_name,50)

sb(16,1) = "{Клиент 1/1    }"
sb(17,1) = "{Клиент 1/2               }"
sb(18,1) = "{Клиент 1/3               }"

tmp_val = sb(15,2)
DIMENSION div_arr(3)
div_arr(1) = LEN(sb(16,1))
div_arr(2) = LEN(sb(17,1))
div_arr(3) = LEN(sb(18,1))
DO Div_Line
sb(16,2) = div_arr(1)
sb(17,2) = div_arr(2)
sb(18,2) = div_arr(3)

sb(19,1) = "{Клиент 2/1                          }"
sb(20,1) = "{Клиент 2/2                          }"

tmp_val = sb(15,2)
DIMENSION div_arr(2)
div_arr(1) = LEN(sb(19,1))
div_arr(2) = LEN(sb(20,1))
DO Div_Line
sb(19,2) = div_arr(1)
sb(20,2) = div_arr(2)

SELECT TIT_PRINT
sb(21,1) = "{Через кого          }"
sb(21,2) = PADR(TIT_PRINT.CONTACT, LEN(sb(21,1)))

sb(22,1) = "{N Накл.  }"
sb(22,2) = IIF(EMPTY(SALE_NUM), "", LEFT(SALE_NUM,4)+"-"+ALLTRIM(RIGHT(SALE_NUM,6)))
sb(22,2) = PADR(sb(22,2), LEN(sb(22,1)))

sb(23,1) = "{Дата Н. }"
sb(23,2) = IIF(EMPTY(SALE_DATE), "          ", DTOC(SALE_DATE))

sb(24,1) = "{N Счета. }"
sb(24,2) = IIF(EMPTY(BILL_NUM), "", LEFT(BILL_NUM,4)+"-"+ALLTRIM(RIGHT(BILL_NUM,6)))
sb(24,2) = PADR(sb(24,2), LEN(sb(24,1)))

sb(25,1) = "{Дата Сч.}"
sb(25,2) = IIF(EMPTY(BILL_DAT), "          ", DTOC(BILL_DAT))

sb(26,1) = "{N Платежа     }"
sb(26,2) = ENTER_DOC
sb(26,2) = PADR(sb(26,2), LEN(sb(26,1)))

sb(27,1) = "{Дата Пл.}"
sb(27,2) = IIF(EMPTY(ENTER_DAT), "          ", DTOC(ENTER_DAT))

sb(28,1) = "{Сумма руб.  }"
sb(28,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(MONEY_R+N_OF_SALE), MONEY_R+N_OF_SALE), "999 999 999.99")

sb(29,1) = "{Сумма USD   }"
sb(29,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(MONEY_D), MONEY_D), "999 999 999.99")

sb(30,1) = "{%Нл}"
sb(30,2) = TRANSFORM(NL_PRC, "999.9")

sb(31,1) = "{Налог       }"
sb(31,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(N_OF_SALE), N_OF_SALE), "999 999 999.99")

sb(32,1) = "{%НД}"
sb(32,2) = TRANSFORM(NDS_, "999.9")

sb(33,1) = "{НДС         }"
DO CASE
CASE tmpDocType $ " Ч"
  sb(33,2) = TRANSFORM(ABS(MONEY_R-MONEY_0), "999 999 999.99")
CASE tmpDocType = "1"
  sb(33,2) = TRANSFORM(MONEY_R-MONEY_0, "999 999 999.99")
OTHERWISE
  sb(33,2) = TRANSFORM(MONEY_D-MONEY_0, "999 999 999.99")
ENDCASE

sb(34,1) = "{Сумма прописью строка 1/1                     }"
sb(35,1) = "{Сумма прописью строка 1/2                     }"
sb(36,1) = "{Сумма прописью строка 1/3                     }"

tmp_val = ABS(IIF(tmpDocType $ "34", MONEY_D+N_OF_SALE, MONEY_R+N_OF_SALE))
tmp_val = Num2RStr(FLOOR(tmp_val))+IIF(tmpDocType $ "34", " У Е ", " руб ")+ ;
          RIGHT(STR(100+(tmp_val-FLOOR(tmp_val))*100),2)+IIF(tmpDocType $ "34", "", " коп.")
tmp_val = Capital(tmp_val)
DIMENSION div_arr(3)
div_arr(1) = LEN(sb(34,1))
div_arr(2) = LEN(sb(35,1))
div_arr(3) = LEN(sb(36,1))
DO Div_Line
sb(34,2) = div_arr(1)
sb(35,2) = div_arr(2)
sb(36,2) = div_arr(3)

sb(37,1) = "{Сумма прописью строка 2/1}"
sb(38,1) = "{Сумма прописью строка 2/2}"
sb(39,1) = "{Сумма прописью строка 2/3}"
sb(40,1) = "{Сумма прописью строка 2/4}"
sb(41,1) = "{Сумма прописью строка 2/5}"

tmp_val = ABS(IIF(tmpDocType $ "34", MONEY_D+N_OF_SALE, MONEY_R+N_OF_SALE))
tmp_val = Num2RStr(FLOOR(tmp_val))+IIF(tmpDocType $ "34", " У Е ", " руб ")+ ;
          RIGHT(STR(100+(tmp_val-FLOOR(tmp_val))*100),2)+IIF(tmpDocType $ "34", "", " коп.")
tmp_val = Capital(tmp_val)
DIMENSION div_arr(5)
div_arr(1) = LEN(sb(37,1))
div_arr(2) = LEN(sb(38,1))
div_arr(3) = LEN(sb(39,1))
div_arr(4) = LEN(sb(40,1))
div_arr(5) = LEN(sb(41,1))
DO Div_Line
sb(37,2) = div_arr(1)
sb(38,2) = div_arr(2)
sb(39,2) = div_arr(3)
sb(40,2) = div_arr(4)
sb(41,2) = div_arr(5)

*
* Изучаем описание документа
DO CASE
CASE tmpDocType = " "
  tmp_val = IIF("-" $ tmpDocNum, "EXP_ORDER", "INC_ORDER")
CASE tmpDocType = "Ч"
  tmp_val = "CHECK"
CASE tmpDocType = "Б"
  tmp_val = "SBERBANK"
CASE tmpDocType = "1"
  tmp_val = "BN_MONEY"
CASE tmpDocType = "3"
  tmp_val = "USD_MONEY"
CASE tmpDocType = "4"
  tmp_val = "USD_BN"
ENDCASE
*------------------------------------
sb(42,1) = "{Autor                       }"

sb(43, 1) = "{Пасп N}"
sb(43, 2) = psp_no

sb(44, 1) = "{ПасС}"
sb(44, 2) = psp_sr

sb(45, 1) = "{Паспорт выдан 1                       }"
sb(45, 2) = psp_l1

sb(46, 1) = "{Паспорт выдан 2                       }"
sb(46, 2) = psp_l2

sb(47, 1) = "{Паспорт выдан 3                       }"
sb(47, 2) = psp_l3

USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(42,2) = "САМ!"
ELSE
  IF SEEK (a_code)
    sb(42,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(42,2) = "?!!"
  ENDIF
ENDIF         
sb(42,2) = PADR(sb(42,2),LEN(sb(42,1)))
*--------------------------------------
USE (base_path+"DOC_FORM")
LOCATE FOR tmp_val == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && Длина страницы в строках
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && Шрифт: " " - обычный, "1" - сжатый (condenced)
n_cp = DOC_FORM.N_COPIES   && Число копий
lft  = DOC_FORM.LEFT_FIELD && Поля слева в символах
ffeed =DOC_FORM.F_FEED     && Завершение документа: 
                           &&      не отрицетельное - прогон строк,
                           &&      отрицательное - прогон формата
p_drctry = DOC_FORM.P_DIR  && Направление печати (принтер)

*
*  Формируем шаблоны для заголовков и подвалов
*
n = MEMLINES(PAGE_H)  &&     Заголовок страницы (не первой)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)  &&     Заголовок первой страницы
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)  &&     Подвал страницы (не последней!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)  &&     Подвал последней страницы
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF
USE

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE str_w    && Образ печатаемой строки

*
*  Заголовок документа
FOR i = 1 TO ALEN(t_d)
  str_w = t_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY str_w
  ELSE
    @ PROW()+1, 0 SAY str_w
  ENDIF
ENDFOR

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.)

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Div_Line     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Разбиение строки...                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.01.1999 ──┘
PROCEDURE Div_Line

PRIVATE s_line, f_line, r_line, n_line, j

n_line = ALEN(div_arr)
s_line = ALLTRIM(tmp_val)
FOR j = 1 TO n_line
  f_line = ""
  r_line = ""
  DO Break_Str WITH s_line, f_line, r_line, div_arr(j)
  div_arr(j) = PADR(ALLTRIM(f_line), div_arr(j))
  s_line = r_line
ENDFOR

RETURN

*************************************************************************
PROCEDURE G_Contact

PRIVATE ln, wd, ex, mss, c1

IF .NOT. EMPTY(face_nam) .AND. .NOT. EMPTY(psp_no)
  RETURN
ENDIF

DO Pasport WITH CUS_CODE, dep_nam, face_nam, psp_sr, psp_no, psp_l1, psp_l2, psp_l3

PRIVATE s_sav

REPLACE CONTACT  WITH face_nam, ; 
        CUS_NAME WITH dep_nam,  ;
        PSP_NUM  WITH psp_sr,   ;
        PSP_SER  WITH psp_no,   ;
        PSP_WH1  WITH psp_l1,   ;
        PSP_WH2  WITH psp_l2,   ;
        PSP_WH3  WITH psp_l3

IF .NOT. EMPTY(SALE_NUM)
  s_sav = SELECT()
  SELECT 0
  USE SALE_TIT ORDER TAG DOC_NUM ALIAS S12345 AGAIN
  IF SEEK(LEFT(DTOS(TIT_PRINT.SALE_DATE),4)+TIT_PRINT.SALE_NUM)
    REPLACE CONTACT WITH face_nam, CUS_NAME WITH dep_nam
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

RETURN .T.
