*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла S_Mail       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Формирование запроса на отправку почты                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 05.02.2006 ══╝
PROCEDURE S_Mail
PARAMETER s_name, c_code, txt_n, dbf_n
*
* s_name - тема сообщения;
* c_code - код клиента;
* txt_n  - имя текстового файла для вложения;
* dbf_n  - имя DBF файла для вложения.
*

PRIVATE tmpAddr, tmpName, tmpText, tmpSw, tmpSub, tmpNot, sv_eml
PRIVATE tmpSwTxt, tmpSwDBF, tmpID, tmpFile, i, sourceEml, mss, sw_addr

m.sv_eml  = .F.
m.tmpAddr = ""
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PS_6205 ORDER TAG CODE IN 0
IF FSIZE("EML", "PS_6205") # 0
  = SEEK(m.user, "PS_6205")
  m.tmpAddr = ALLTRIM(PS_6205.EML)
ELSE
  m.tmpAddr = PADR(PersParm("MAILADDR"), 60)
ENDIF
USE IN PS_6205

m.tmpName = PADR(PersParm("MAILNAME"), 60)
m.tmpNot  = .NOT. EMPTY(PersParm("NOTIFICAT"))
m.tmpText = PersParm("MAILTEXT")
m.tmpSw   = VAL(PersParm("MAILMODE"))
m.tmpSw   = MAX(m.tmpSw, 1)
m.tmpSub  = PADR(s_name, 60)
m.sw_addr = .NOT. EMPTY(PersParm("MAILTOSELF"))

PRIVATE to_eml, to_nam, s_sav
m.to_eml = ""
m.sourceEml = ""
m.to_nam = ""
m.tmpSwTxt = .T.
m.tmpSwDBF = .NOT. EMPTY(dbf_n)

s_sav = SELECT()
SELECT 0
IF .NOT. EMPTY(m.c_code)
  USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
  IF SEEK(m.c_code)
    m.to_nam = ALLTRIM(CUS_NAME)
  ENDIF
  USE (m.base_path+"CLI_INFO") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
  IF SEEK(m.c_code)
    m.to_eml = ALLTRIM(EML)
    m.sourceEml = m.to_eml
  ENDIF
  USE
ENDIF

m.to_eml = PADR(m.to_eml, 60)
m.to_nam = PADR(m.to_nam, 50)

IF m.tmpSw # 2 .OR. EMPTY(m.to_eml)	.OR. m.tmpSwDBF		&& Заставка всегда или нет адреса
  IF .NOT. Interact()
    SELECT (m.s_sav)
    RETURN
  ENDIF
ENDIF

USE (m.send_log+"MAILPRM") SHARED AGAIN
REPLACE MAILCOUNT WITH MAILCOUNT+1
m.tmpID = MAILCOUNT
USE
m.tmpID = RIGHT(STR(m.tmpID*69489693, 20), 8)
m.tmpID = PADL(ALLTRIM(m.tmpID), 8, "0")

IF .NOT. Is_Dir(m.send_log+"MAIL")		&& Нет директории для вложений - создадим!
  m.tmpFile = SET("DEFAULT")+CURDIR()
  SET DEFAULT TO (m.send_log)
  ! MD "MAIL"
  SET DEFAULT TO (m.tmpFile)
ENDIF

IF m.tmpSwTxt	&& Вкладываем TXT файл
  IF File_O(m.txt_n)
    COPY FILE (m.txt_n) TO (m.send_log+"MAIL\"+m.tmpID+".txt")
  ENDIF
ENDIF

IF m.tmpSwDBF	&& Вкладываем DBF файл
  m.i = RAT(".", m.dbf_n)
  IF m.i = 0
    m.tmpFile = ALLTRIM(m.dbf_n)
  ELSE
    m.tmpFile = ALLTRIM(LEFT(m.dbf_n, m.i-1))
  ENDIF

  IF File_O(m.tmpFile+".dbf")
    COPY FILE (m.tmpFile+".dbf") TO (m.send_log+"MAIL\"+m.tmpID+".dbf")
  ENDIF

  IF File_O(m.tmpFile+".fpt")
    COPY FILE (m.tmpFile+".fpt") TO (m.send_log+"MAIL\"+m.tmpID+".fpt")
  ENDIF

  IF File_O(m.tmpFile+".cdx")
    COPY FILE (m.tmpFile+".cdx") TO (m.send_log+"MAIL\"+m.tmpID+".cdx")
  ENDIF

ENDIF

USE (m.send_log+"MAILSEND") SHARED AGAIN
APPEND BLANK
REPLACE	LETTERID	WITH m.tmpID,	;
		SUBJECT		WITH m.tmpSub,	;
		NOTIFY		WITH m.tmpNot,	;
		TO_SELF		WITH m.sw_addr,	;
		LETT_TEXT	WITH m.tmpText,	;
		CUS_NAME	WITH m.to_nam,	;
		CUS_MAIL	WITH m.to_eml,	;
		AUTHOR_NAM	WITH m.tmpName,	;
		AUTHOR_EML	WITH m.tmpAddr,	;
		DATE_CR		WITH DATE(),	;
		TIME_CR		WITH TIME(),	;
		WHO_CR		WITH m.user

REPLACE STATUS WITH "*"
USE (m.base_path+"SERVERS") SHARED AGAIN ORDER TAG SERV_ACT DESCENDING
IF .NOT. SEEK(PADR("E-MAIL", FSIZE("SERV_TYPE")))
  DIMENSION mss[4]
  mss[1] = ""
  mss[2] = CHR(0)+"ВНИМАНИЕ! Ваше письмо не будет отправлено,"
  mss[3] = CHR(0)+"пока не будет запущен сервер электронной почты"
  mss[4] = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  IF START_DATE # DATE()
    DIMENSION mss[4]
    mss[1] = ""
    mss[2] = CHR(0)+"ВНИМАНИЕ! Похоже что сервер электронной почты не запушен."
    mss[3] = CHR(0)+"Уточните, что с ним, иначе Ваше письмо не будет оправлено."
    mss[4] = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
USE

SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Interact     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Интерактивное заполнение параметров                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.02.2006 ──┘
PROCEDURE Interact

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────────────────────────────────┐
*│┌─ Получатель ──────────────────────────────────────────────────────┐│
*││      Имя ....:....!....:....!....:....!....:....!....:....!       ││
*││                                                                   ││
*││E-mail ....:....!....:....!....:....!....:....!....:....!....:....!││
*│└───────────────────[ ] Сохранить E-mail клиента────────────────────┘│
*│                     [ ] Уведомление о прочтении                     │
*│Тема ....:....!....:....!....:....!....:....!....:....!....:. <Выбор>│
*│┌─ Текст письма ────────────────────────────────────────────────────┐│
*││                                                                   ││
*││                                                                   ││
*││                                                                   ││
*││                                                                   ││
*││                                                                   ││
*│└───────────────────────────────────────────────────────────────────┘│
*│                    Вложения: [ ] Текст   [ ] DBF                    │
*│                    < OK Ctrl-W > < Отказаться Esc >                 │
*└─────────────────────────────────────────────────────────────────────┘

PRIVATE ex, tmpNm, tmpCh
m.ex = 1
m.tmpCh = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH IIF(m.tmpSwDBF, 19, 18), 73, "Отправка письма по электронной почте"
DO Sun_Bord WITH  2,  2,  6, 70, " Получатель "
DO Sun_Bord WITH  9,  2, 15, 70, " Текст письма "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  9 SAY "Имя" GET m.to_nam
@  5,  3 SAY "E-mail" GET m.to_eml VALID S_Flag()
@  6, 22 GET m.sv_eml PICTURE "@*C Сохранить E-mail клиента" ;
               WHEN .NOT. EMPTY(c_code) .AND. .NOT. m.sourceEml == ALLTRIM(m.to_eml)
@  7, 23 GET m.tmpNot PICTURE "@*C Уведомление о прочтении"
@  8,  2 SAY "Тема" GET m.tmpSub PICTURE "@S56"
@  8, 64 GET m.tmpCh PICTURE "@*N Выбор" VALID LettList()
@ 10,  3 EDIT m.tmpText SIZE 5, 67 COLOR (SCHEME(1, 1))
IF m.tmpSwDBF
  @ WROWS()-3, 22 SAY "Вложения:" GET m.tmpSwTxt PICTURE "@*C Текст"
  @ WROWS()-3, 44 GET m.tmpSwDBF PICTURE "@*C DBF"
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Eml()

IF m.ex = 1
  *
  * Отрабатываем бланк: сохраняем адрес
  IF m.sv_eml .AND. .NOT. EMPTY(m.c_code) .AND. .NOT. EMPTY(m.to_eml)
    USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
    IF SEEK(m.c_code)
      m.tmpNm = ALLTRIM(CUS_NAME)
    ENDIF
    USE (m.base_path+"CLI_INFO") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
    IF .NOT. SEEK(m.c_code)
      APPEND BLANK
      REPLACE CUS_CODE WITH m.c_code, ;
              NAME     WITH m.tmpNm
    ENDIF
    REPLACE EML WITH m.to_eml
    USE
  ENDIF

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Eml      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности бланка                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.02.2006 ──┘
PROCEDURE Tst_Eml

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.to_eml)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Почтовый адрес должен быть указан..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Flag       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Флажок сохранения адреса                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.02.2006 ──┘
PROCEDURE S_Flag

m.sv_eml = .NOT. EMPTY(c_code) .AND. .NOT. m.sourceEml == ALLTRIM(m.to_eml)
SHOW GET m.sv_eml 

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура LettList     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Выбор заготовки письма                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.04.2006 ──┘
PROCEDURE LettList

PRIVATE tSub, tText

m.tSub  = ""
m.tText = ""
DO Ch_Lett WITH m.tSub, m.tText

IF .NOT. EMPTY(m.tSub)
  m.tmpSub = m.tSub
  SHOW GET m.tmpSub
ENDIF

IF .NOT. EMPTY(m.tText)
  m.tmpText = m.tText
  SHOW GET m.tmpText
ENDIF

RETURN .T.
