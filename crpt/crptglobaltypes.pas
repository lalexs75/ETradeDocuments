{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit CrptGlobalTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCRPTDocumentFormat = (cdfManual, cdfUpd, cdfXml, cdfCsv);
  TCRPTDocumentFormats = set of TCRPTDocumentFormat;

  TDocumentType = (
    AGGREGATION_DOCUMENT, AGGREGATION_DOCUMENT_CSV, AGGREGATION_DOCUMENT_XML,
    DISAGGREGATION_DOCUMENT, DISAGGREGATION_DOCUMENT_CSV, DISAGGREGATION_DOCUMENT_XML,
    REAGGREGATION_DOCUMENT, REAGGREGATION_DOCUMENT_XML, REAGGREGATION_DOCUMENT_CSV,
    LP_INTRODUCE_GOODS, LP_INTRODUCE_GOODS_CSV, LP_INTRODUCE_GOODS_XML,
    LP_SHIP_GOODS, LP_SHIP_GOODS_CSV, LP_SHIP_GOODS_XML,
    LP_SHIP_RECEIPT, LP_SHIP_RECEIPT_CSV, CSVLP_SHIP_RECEIPT_XML,
    LP_SHIP_GOODS_CROSSBORDER, LP_ACCEPT_GOODS, LP_ACCEPT_GOODS_XML,
    LK_REMARK, LK_REMARK_CSV, LK_REMARK_XML,
    LP_GOODS_IMPORT, LP_GOODS_IMPORT_CSV, LP_GOODS_IMPORT_XML,
    LP_CANCEL_SHIPMENT, LP_CANCEL_SHIPMENT_CROSSBORDER,
    LK_KM_CANCELLATION, LK_KM_CANCELLATION_XML,

    LK_KM_CANCELLATION_CSV, LK_APPLIED_KM_CANCELLATION, LK_APPLIED_KM_CANCELLATION_XML,
    LK_APPLIED_KM_CANCELLATION_CSV, LK_CONTRACT_COMMISSIONING, LK_CONTRACT_COMMISSIONING_CSV, LK_CONTRACT_COMMISSIONING_XML,
    LK_INDI_COMMISSIONING, LK_INDI_COMMISSIONING_CSV, CSVLK_INDI_COMMISSIONING_XML,
    LP_RETURN, LP_RETURN_CSV, LP_RETURN_XML,
    OST_DESCRIPTION, OST_DESCRIPTION_CSV, OST_DESCRIPTION_XML,

    LP_INTRODUCE_OST, JSONLP_INTRODUCE_OST_CSV, LP_INTRODUCE_OST_XML,
    CROSSBORDER, CROSSBORDER_CSV, CROSSBORDER_XML,
    LK_RECEIPT, LK_RECEIPT_CSV, LK_RECEIPT_XML,
    LP_INTRODUCE_GOODS_CROSSBORDER_CSD_JSON, LP_INTRODUCE_GOODS_CROSSBORDER_CSD_XML, LP_INTRODUCE_GOODS_CROSSBORDER_CSD_CSV,
    CSVLP_FTS_INTRODUCE_JSON, LP_FTS_INTRODUCE_XML, LP_FTS_INTRODUCE_CSV,
    ATK_AGGREGATION, ATK_AGGREGATION_CSV, ATK_AGGREGATION_XML,
    ATK_TRANSFORMATION, ATK_TRANSFORMATION_CSV, ATK_TRANSFORMATION_XML,
    ATK_DISAGGREGATION, ATK_DISAGGREGATION_CSV, ATK_DISAGGREGATION_XML,
    XMLRECEIPT, RECEIPT_RETURN,
    UNIVERSAL_TRANSFER_DOCUMENT, UNIVERSAL_TRANSFER_DOCUMENT_FIX, UNIVERSAL_CORRECTION_DOCUMENT,
    UNIVERSAL_CORRECTION_DOCUMENT_FIX, UNIVERSAL_CANCEL_DOCUMENT);
  TDocumentTypes = set of TDocumentType;

  TCrptDocListFilter = record
    DateFrom:TDateTime;
    DateTo:TDateTime;
    DocumentFormat:TCRPTDocumentFormats;
    Limit:Integer;
  end;

implementation

end.

