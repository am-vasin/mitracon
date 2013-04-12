*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл men_rep      Разработчик Андрей Васин           11.02.98 15:23:30 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Формирование DBF-файла отчета по сотрудникам.              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Men_Rep

*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────┐
*│ ┌─ Сотрудник ────────────────────────────┐ │
*│ │ ( ) Фамилия, Имя, Отчество             │ │
*│ │ ( ) Фамилия Имя Отчество               │ │
*│ │ ( ) Фамилия и инициалы                 │ │
*│ │ [ ] Фамилия   [ ] Имя   [ ] Отчество   │ │
*│ └────────────────────────────────────────┘ │
*│ ┌─ Состав полей ─────────────────────────┐ │
*│ │ [ ] Отдел          [ ] Должность       │ │
*│ │ [ ] Площадка       [ ] Комната         │ │
*│ │ [ ] Раб. телефон   [ ] Местн. телефон  │ │
*│ │ [ ] Дом. адрес     [ ] Дом. телефон    │ │
*│ │ [ ] Дата рождения  [ ] Пост. на работу │ │
*│ └────────────────────────────────────────┘ │
*│ ┌─ Упорядочить по ───────────────────────┐ │
*│ │   ( ) Фамилии  ( ) Отделу и фамилии    │ │
*│ └────────────────────────────────────────┘ │
*│      < OK Ctrl-W > < Отказаться Esc >      │
*└────────────────────────────────────────────┘

PRIVATE ex, sw_name
PRIVATE name1, name2, name3
PRIVATE sw_dep, sw_pos, sw_phone1, sw_phone2, sw_place
PRIVATE sw_room, sw_addr, sw_phone_h, sw_birth, sw_d_in
PRIVATE sw_ord

ex   = 1
sw_name = 1
name1 = .T.
name2 = .T.
name3 = .T.
STORE .T. TO sw_dep, sw_pos, sw_phone1, sw_phone2, sw_place
STORE .T. TO sw_room, sw_addr, sw_phone_h, sw_birth, sw_d_in
sw_ord = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 20, 48, "Состав отчета"
DO Sun_Bord WITH 2, 3, 7, 44, " Сотрудник "
DO Sun_Bord WITH 8, 3, 14, 44, " Состав полей "
DO Sun_Bord WITH 15, 3, 17, 44, " Упорядочить по "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  5 GET sw_name PICTURE    ;
      "@*RV Фамилия, Имя, Отчество;Фамилия Имя Отчество;Фамилия и инициалы" ;
      VALID Set_D()
@  6,  5 GET name1 PICTURE "@*C Фамилия" WHEN sw_name = 1
@  6, 19 GET name2 PICTURE "@*C Имя" WHEN sw_name = 1
@  6, 29 GET name3 PICTURE "@*C Отчество" WHEN sw_name = 1
@  9,  5 GET sw_dep PICTURE "@*C Отдел"
@  9, 24 GET sw_pos PICTURE "@*C Должность"
@ 10,  5 GET sw_place PICTURE "@*C Площадка"
@ 10, 24 GET sw_room PICTURE "@*C Комната"
@ 11,  5 GET sw_phone1 PICTURE "@*C Раб. телефон"
@ 11, 24 GET sw_phone2 PICTURE "@*C Местн. телефон"
@ 12,  5 GET sw_addr PICTURE "@*C Дом. адрес"
@ 12, 24 GET sw_phone_h PICTURE "@*C Дом. телефон"
@ 13,  5 GET sw_birth PICTURE "@*C Дата рождения"
@ 13, 24 GET sw_d_in PICTURE "@*C Пост. на работу"
@ 16,  7 GET sw_ord PICTURE "@*RH Фамилии;Отделу и фамилии"
@ 18,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

RELEASE WINDOW (win_name)
POP KEY

IF ex = 2
  RETURN
ENDIF
*--------------------------------------------------------------------------
DO Prep_H
IF File_O(tmpo_path+"RES_REP.DBF")
  PRIVATE new_f, f_sz, a_w
  = ADIR(a_w, tmpo_path+"RES_REP.DBF")
  f_sz = a_w(1,2)
  new_f = New_File("", "DBF", "Куда пишем DBF-файл?", f_sz) 

  IF .NOT. EMPTY(new_f)
    DO Wt_Mess WITH "Запись..."
    new_f = ALLTRIM(new_f)
    COPY FILE (tmpo_path+"RES_REP.DBF") TO (new_f)
    DO Wt_Mess
  ENDIF
  DELETE FILE (tmpo_path+"RES_REP.DBF")
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║             Очистка флажков "Фамилия", "Имя", "Отчество".              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.02.98 17:30:14 ═╝
PROCEDURE Set_D

IF sw_name # 1
  name1 = .F.
  name2 = .F.
  name3 = .F.
  SHOW GET name1
  SHOW GET name2
  SHOW GET name3
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Подготовка полного файла отчета.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.02.98 17:37:47 ═╝
PROCEDURE Prep_H
PRIVATE f_name

USE (base_path+"DEPS") ORDER TAG CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME
SET RELATION TO DEP_NO INTO DEPS

SELECT 0
CREATE DBF (tmpo_path+"HOLE_REP") ;
  ( FAMILY   C(FSIZE("FAMILY", "PERSONS")),   ;
    NAME     C(FSIZE("NAME",   "PERSONS")),   ;
    S_NAME   C(FSIZE("S_NAME", "PERSONS")),   ;
    FIO      C(FSIZE("FAMILY", "PERSONS")+    ;
               FSIZE("NAME",   "PERSONS")+    ;
               FSIZE("S_NAME", "PERSONS")+2), ;
    FAM_IO   C(FSIZE("NAME",   "PERSONS")+6), ;
    DEP_CODE N(3), ;
    DEP_NAME C(FSIZE("L_NAME",  "DEPS")),     ;
    POSITION C(FSIZE("POSITION","PERSONS")),  ;
    PLACE    C(FSIZE("PLACE",   "PERSONS")),  ;
    ROOM     C(FSIZE("ROOM",    "PERSONS")),  ;
    PHONE    C(FSIZE("W_PHONE1","PERSONS")),  ;
    L_PHONE  C(FSIZE("W_PHONE2","PERSONS")),  ;
    ADDRESS  C(FSIZE("H_ADDRESS","PERSONS")), ;
    BIRTHDAY C(10),  ;
    DATE_ON  C(10)  )

SELECT PERSONS
SCAN
  SELECT HOLE_REP
  APPEND BLANK
  REPLACE FAMILY   WITH PERSONS.FAMILY, ;
          NAME     WITH PERSONS.NAME,   ;
          S_NAME   WITH PERSONS.S_NAME, ;
          FIO      WITH ALLTRIM(PERSONS.FAMILY)+" "+    ;
                        ALLTRIM(PERSONS.NAME)+" "+      ;
                        ALLTRIM(PERSONS.S_NAME),        ;
          FAM_IO   WITH ALLTRIM(PERSONS.FAMILY)+" "+    ;
                        LEFT(PERSONS.NAME,1)+". "+      ;
                        LEFT(PERSONS.S_NAME,1)+".",     ;
          DEP_CODE WITH PERSONS.DEP_NO,    ;
          DEP_NAME WITH DEPS.L_NAME,       ;
          POSITION WITH PERSONS.POSITION,  ;
          PLACE    WITH PERSONS.PLACE,     ;
          ROOM     WITH PERSONS.ROOM,      ;
          PHONE    WITH PERSONS.W_PHONE1,  ;
          L_PHONE  WITH PERSONS.W_PHONE2,  ;
          ADDRESS  WITH PERSONS.H_ADDRESS, ;
          BIRTHDAY WITH DTOC(PERSONS.BIRTHDAY),  ;
          DATE_ON  WITH DTOC(PERSONS.DATE_IN)
  SELECT PERSONS
ENDSCAN
USE
SELECT DEPS
USE
SELECT HOLE_REP
USE (tmpo_path+"HOLE_REP") EXCLU
IF sw_ord = 2
  INDEX ON STR(DEP_CODE,3)+FAMILY+NAME+S_NAME TAG ORD
ENDIF

f_name = ""
DO CASE
CASE sw_name = 1
  f_name = f_name+IIF(name1, "FAMILY,", "")+ ;
                  IIF(name2, "NAME,", "")+ ;
                  IIF(name1, "S_NAME,", "")
CASE sw_name = 2
  f_name = f_name+"FIO,"
CASE sw_name = 3
  f_name = f_name+"FAM_IO,"
ENDCASE

f_name = f_name+IIF(sw_dep,"DEP_NAME,","")+ ;
                IIF(sw_pos,"POSITION,","")+ ;
                IIF(sw_place,"PLACE,","")+ ;
                IIF(sw_room,"ROOM,","")+ ;
                IIF(sw_phone1,"PHONE,","")+ ;
                IIF(sw_phone2,"L_PHONE,","")+ ;
                IIF(sw_addr,"ADDRESS,","")+ ;
                IIF(sw_birth,"BIRTHDAY,","")+ ;
                IIF(sw_d_in,"DATE_ON,","")

IF EMPTY(f_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали ни одного поля!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  f_name = LEFT(f_name,LEN(f_name)-1)
  COPY TO (tmpo_path+"RES_REP") FIELDS &f_name
ENDIF
USE
DELETE FILE (tmpo_path+"HOLE_REP.DBF")
DELETE FILE (tmpo_path+"HOLE_REP.CDX")

RETURN
