*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/29/2021 at 11:05:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0V_SFUT_RECV.................................*
TABLES: ZMA0V_SFUT_RECV, *ZMA0V_SFUT_RECV. "view work areas
CONTROLS: TCTRL_ZMA0V_SFUT_RECV
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMA0V_SFUT_RECV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMA0V_SFUT_RECV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMA0V_SFUT_RECV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_RECV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_RECV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMA0V_SFUT_RECV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMA0V_SFUT_RECV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMA0V_SFUT_RECV_TOTAL.

*.........table declarations:.................................*
TABLES: LFA1                           .
TABLES: T001W                          .
TABLES: T024E                          .
TABLES: ZMA0_SFUT_EMAIL                .
TABLES: ZMA0_SFUT_RECV                 .
