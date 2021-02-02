*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/29/2021 at 11:04:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0V_SFUT_REAS.................................*
TABLES: ZMA0V_SFUT_REAS, *ZMA0V_SFUT_REAS. "view work areas
CONTROLS: TCTRL_ZMA0V_SFUT_REAS
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMA0V_SFUT_REAS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMA0V_SFUT_REAS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMA0V_SFUT_REAS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_REAS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_REAS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMA0V_SFUT_REAS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_REAS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_REAS_TOTAL.

*.........table declarations:.................................*
TABLES: T024E                          .
TABLES: ZMA0_SFUT_REAS                 .
