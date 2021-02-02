*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/29/2021 at 11:00:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0V_SFUT_EMAIL................................*
TABLES: ZMA0V_SFUT_EMAIL, *ZMA0V_SFUT_EMAIL. "view work areas
CONTROLS: TCTRL_ZMA0V_SFUT_EMAIL
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMA0V_SFUT_EMAIL. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMA0V_SFUT_EMAIL.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMA0V_SFUT_EMAIL_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_EMAIL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_EMAIL_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMA0V_SFUT_EMAIL_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_EMAIL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_EMAIL_TOTAL.

*.........table declarations:.................................*
TABLES: T024E                          .
TABLES: ZMA0_SFUT_EMAIL                .
