*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 01/29/2021 at 11:04:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0V_SFUT_REAS.................................*
FORM GET_DATA_ZMA0V_SFUT_REAS.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZMA0_SFUT_REAS WHERE
(VIM_WHERETAB) .
    CLEAR ZMA0V_SFUT_REAS .
ZMA0V_SFUT_REAS-MANDT =
ZMA0_SFUT_REAS-MANDT .
ZMA0V_SFUT_REAS-EKORG =
ZMA0_SFUT_REAS-EKORG .
ZMA0V_SFUT_REAS-LANGU =
ZMA0_SFUT_REAS-LANGU .
ZMA0V_SFUT_REAS-REASON =
ZMA0_SFUT_REAS-REASON .
    SELECT SINGLE * FROM T024E WHERE
EKORG = ZMA0_SFUT_REAS-EKORG .
    IF SY-SUBRC EQ 0.
ZMA0V_SFUT_REAS-EKOTX =
T024E-EKOTX .
    ENDIF.
<VIM_TOTAL_STRUC> = ZMA0V_SFUT_REAS.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZMA0V_SFUT_REAS .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZMA0V_SFUT_REAS.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZMA0V_SFUT_REAS-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZMA0_SFUT_REAS WHERE
  EKORG = ZMA0V_SFUT_REAS-EKORG AND
  LANGU = ZMA0V_SFUT_REAS-LANGU AND
  REASON = ZMA0V_SFUT_REAS-REASON .
    IF SY-SUBRC = 0.
    DELETE ZMA0_SFUT_REAS .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZMA0_SFUT_REAS WHERE
  EKORG = ZMA0V_SFUT_REAS-EKORG AND
  LANGU = ZMA0V_SFUT_REAS-LANGU AND
  REASON = ZMA0V_SFUT_REAS-REASON .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZMA0_SFUT_REAS.
    ENDIF.
ZMA0_SFUT_REAS-MANDT =
ZMA0V_SFUT_REAS-MANDT .
ZMA0_SFUT_REAS-EKORG =
ZMA0V_SFUT_REAS-EKORG .
ZMA0_SFUT_REAS-LANGU =
ZMA0V_SFUT_REAS-LANGU .
ZMA0_SFUT_REAS-REASON =
ZMA0V_SFUT_REAS-REASON .
    IF SY-SUBRC = 0.
    UPDATE ZMA0_SFUT_REAS .
    ELSE.
    INSERT ZMA0_SFUT_REAS .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZMA0V_SFUT_REAS-UPD_FLAG,
STATUS_ZMA0V_SFUT_REAS-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZMA0V_SFUT_REAS.
  SELECT SINGLE * FROM ZMA0_SFUT_REAS WHERE
EKORG = ZMA0V_SFUT_REAS-EKORG AND
LANGU = ZMA0V_SFUT_REAS-LANGU AND
REASON = ZMA0V_SFUT_REAS-REASON .
ZMA0V_SFUT_REAS-MANDT =
ZMA0_SFUT_REAS-MANDT .
ZMA0V_SFUT_REAS-EKORG =
ZMA0_SFUT_REAS-EKORG .
ZMA0V_SFUT_REAS-LANGU =
ZMA0_SFUT_REAS-LANGU .
ZMA0V_SFUT_REAS-REASON =
ZMA0_SFUT_REAS-REASON .
    SELECT SINGLE * FROM T024E WHERE
EKORG = ZMA0_SFUT_REAS-EKORG .
    IF SY-SUBRC EQ 0.
ZMA0V_SFUT_REAS-EKOTX =
T024E-EKOTX .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMA0V_SFUT_REAS-EKOTX .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZMA0V_SFUT_REAS USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZMA0V_SFUT_REAS-EKORG TO
ZMA0_SFUT_REAS-EKORG .
MOVE ZMA0V_SFUT_REAS-LANGU TO
ZMA0_SFUT_REAS-LANGU .
MOVE ZMA0V_SFUT_REAS-REASON TO
ZMA0_SFUT_REAS-REASON .
MOVE ZMA0V_SFUT_REAS-MANDT TO
ZMA0_SFUT_REAS-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZMA0_SFUT_REAS'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZMA0_SFUT_REAS TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZMA0_SFUT_REAS'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZMA0V_SFUT_REAS USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZMA0_SFUT_REAS-MANDT =
ZMA0V_SFUT_REAS-MANDT .
ZMA0_SFUT_REAS-EKORG =
ZMA0V_SFUT_REAS-EKORG .
ZMA0_SFUT_REAS-LANGU =
ZMA0V_SFUT_REAS-LANGU .
ZMA0_SFUT_REAS-REASON =
ZMA0V_SFUT_REAS-REASON .
    SELECT SINGLE * FROM T024E WHERE
EKORG = ZMA0_SFUT_REAS-EKORG .
    IF SY-SUBRC EQ 0.
ZMA0V_SFUT_REAS-EKOTX =
T024E-EKOTX .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMA0V_SFUT_REAS-EKOTX .
    ENDIF.
ENDFORM.
