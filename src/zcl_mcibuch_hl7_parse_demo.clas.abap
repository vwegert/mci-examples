"! <strong>Beispiel zur Verwendung des HL7-Parsers</strong>
"! <br/>
"! Verwenden Sie die Transaktion ZMCIBUCH_HL7_PARSE, um dieses Beispiel
"! auszuführen.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 5.2.5
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_hl7_parse_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    "! Setzt eine HL7-Textnachricht in das Objektmodell um und zeigt sie an.
    METHODS execute.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "! Stellt eine HL7-Nachricht als Text bereit.
    METHODS create_hl7_text_message
      RETURNING
        VALUE(r_result) TYPE string.
ENDCLASS.



CLASS ZCL_MCIBUCH_HL7_PARSE_DEMO IMPLEMENTATION.


  METHOD create_hl7_text_message.
    r_result = 'MSH|^~\&|ISHMED|XT1.200|||20171108150531||ADT^A28^ADT_A05|123456|D|2.5.1|||AL|NE|US|UNICODE|EN^English^ISO639||' &&
               cl_abap_char_utilities=>cr_lf &&
               'EVN||20171108150450||O|||' &&
               cl_abap_char_utilities=>cr_lf &&
               'PID|1||0002001475^^^^PI||Mittwoch^Michaela^^^^^L^A^^^G~Mittwoch^^^^^^M^A||19600504|F|||||||EN^English^ISO639|||||||||||US^amerikanisch^ISO3166||||N|||||||||' &&
               cl_abap_char_utilities=>cr_lf &&
               'PV1|1|N|||||||||||||||||^^^^BN|||||||||||||||||||||||||||||||||' &&
               cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.


  METHOD execute.

    " HL7-Nachricht in Textform (hier: HL7-Pipe) beschaffen
    DATA(hl7_text_message) = create_hl7_text_message( ).

    " Protokoll bereitstellen
    DATA(errorhandler) = NEW cl_ishmed_errorhandling( ).
    DATA(logger) = NEW cl_ishmed_mci_log_errorhandler(
        ir_errorhandler = errorhandler
    ).

    TRY.
        " HL7-Parser initialisieren
        DATA(hl7_parser) = CAST if_ishmed_mci_hl7_parser(
                             NEW cl_ishmed_mci_hl7_pipe_parser( ) ).
        hl7_parser->init(
            i_type    = if_ishmed_mci_comp_constant=>co_parser
            ir_logger = logger
         ).

        " Nachricht zum Parsen übergeben
        DATA(hl7_message) = hl7_parser->parse(
            i_value   = hl7_text_message
            ir_logger = logger
        ).

        " Nachricht zur Demonstration anzeigen
        IF hl7_message IS NOT INITIAL.
          hl7_message->if_ishmed_mci_message~display( ).
        ENDIF.

        " Instanzen wieder abbauen
        hl7_message->if_ishmed_object~finalize( ).
        hl7_parser->if_ishmed_object~finalize( ).
        logger->if_ishmed_object~finalize( ).

      CATCH cx_ishmed_object cx_ishmed_mci INTO DATA(exception).
        errorhandler->append_message_for_exception( exception ).
    ENDTRY.

    " Meldungen anzeigen
    errorhandler->display_messages( ).

  ENDMETHOD.
ENDCLASS.
