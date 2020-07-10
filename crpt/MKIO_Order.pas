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

unit MKIO_Order;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types, AbstractSerializationObjects;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  TPREP_METHOD = Longint;
  TRECEPTION_METHOD = Longint;
  TGTIN = String;
  TTNVED_CODE = Int64;
  TRELEASE_METHOD = Longint;
  TIO_TYPE = Longint;
  TS_ORG_METHOD = Longint;
  TMARKING_TYPE = Longint;
  TSERIAL_NUMBER = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  TMKIO_Order = class;
  TMKIO_Order_element = class;
  TMKIO_Order_PRODUCT_DETAILS = class;
  TMKIO_Order_PRODUCT_DETAILS_PRODUCT = class;
  TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  TMKIO_OrderList = specialize GXMLSerializationObjectList<TMKIO_Order>;
  TMKIO_Order_elementList = specialize GXMLSerializationObjectList<TMKIO_Order_element>;
  TMKIO_Order_PRODUCT_DETAILSList = specialize GXMLSerializationObjectList<TMKIO_Order_PRODUCT_DETAILS>;
  TMKIO_Order_PRODUCT_DETAILS_PRODUCTList = specialize GXMLSerializationObjectList<TMKIO_Order_PRODUCT_DETAILS_PRODUCT>;
  TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMSList = specialize GXMLSerializationObjectList<TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS>;

  {  Tfias_address_type  }
  //Адрес в формате ФИАС
  Tfias_address_type = class(TXmlSerializationObject)
  private
    Faoguid:Taoguid;
    Fhouseguid:Thouseguid;
    Fflat:Tflat;
    procedure Setaoguid( AValue:Taoguid);
    procedure Sethouseguid( AValue:Thouseguid);
    procedure Setflat( AValue:Tflat);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Глобальный уникальный идентификатор адресного объекта
    property aoguid:Taoguid read Faoguid write Setaoguid;
    //Глобальный уникальный идентификатор домаОбязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //КвартираОбязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

  {  TMKIO_Order  }
  //Заказ КМ СИ
  TMKIO_Order = class(TXmlSerializationObject)
  private
    FTRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type;
    FOPERATOR_CONTRACT_NUM:Tstring255_type;
    FOPERATOR_CONTRACT_DATE:Tdate_type;
    FPREP_METHOD:TPREP_METHOD;
    FRECEPTION_METHOD:TRECEPTION_METHOD;
    FLABEL_TEMPLATE_ID:Tguid_type;
    FLABELLING_CENTER_NAME:Tstring255_type;
    FLABELLING_CENTER_INN:TLABELLING_CENTER_INN_type;
    FLABELLING_CENTER_CONTRACT_NUM:Tstring255_type;
    FLABELLING_CENTER_CONTRACT_DATE:Tdate_type;
    FCONTACT:Tstring255_type;
    FDELIVERY_ADDRESS:Tstring255_type;
    FPRODUCT_DETAILS:TMKIO_Order_PRODUCT_DETAILS;
    Faction_id:Longint;
    procedure SetTRADE_PARTICIPANT_INN( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure SetOPERATOR_CONTRACT_NUM( AValue:Tstring255_type);
    procedure SetOPERATOR_CONTRACT_DATE( AValue:Tdate_type);
    procedure SetPREP_METHOD( AValue:TPREP_METHOD);
    procedure SetRECEPTION_METHOD( AValue:TRECEPTION_METHOD);
    procedure SetLABEL_TEMPLATE_ID( AValue:Tguid_type);
    procedure SetLABELLING_CENTER_NAME( AValue:Tstring255_type);
    procedure SetLABELLING_CENTER_INN( AValue:TLABELLING_CENTER_INN_type);
    procedure SetLABELLING_CENTER_CONTRACT_NUM( AValue:Tstring255_type);
    procedure SetLABELLING_CENTER_CONTRACT_DATE( AValue:Tdate_type);
    procedure SetCONTACT( AValue:Tstring255_type);
    procedure SetDELIVERY_ADDRESS( AValue:Tstring255_type);
    procedure Setaction_id( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН Участника оборота
    property TRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type read FTRADE_PARTICIPANT_INN write SetTRADE_PARTICIPANT_INN;
    //Номер договора с Оператором ИС МП
    property OPERATOR_CONTRACT_NUM:Tstring255_type read FOPERATOR_CONTRACT_NUM write SetOPERATOR_CONTRACT_NUM;
    //Дата договора с Оператором ИС МП
    property OPERATOR_CONTRACT_DATE:Tdate_type read FOPERATOR_CONTRACT_DATE write SetOPERATOR_CONTRACT_DATE;
    //Способ изготовления
    //1 - Самостоятельно
    //2 - ЦЭМ
    property PREP_METHOD:TPREP_METHOD read FPREP_METHOD write SetPREP_METHOD;
    //Способ получения
    //1 - На физическом носителе
    //2 - В электронном виде
    property RECEPTION_METHOD:TRECEPTION_METHOD read FRECEPTION_METHOD write SetRECEPTION_METHOD;
    //ID шаблона этикеток
    property LABEL_TEMPLATE_ID:Tguid_type read FLABEL_TEMPLATE_ID write SetLABEL_TEMPLATE_ID;
    //Наименование ЦЭМ
    property LABELLING_CENTER_NAME:Tstring255_type read FLABELLING_CENTER_NAME write SetLABELLING_CENTER_NAME;
    //ИНН ЦЭМ
    property LABELLING_CENTER_INN:TLABELLING_CENTER_INN_type read FLABELLING_CENTER_INN write SetLABELLING_CENTER_INN;
    //Номер договора с ЦЭМ
    property LABELLING_CENTER_CONTRACT_NUM:Tstring255_type read FLABELLING_CENTER_CONTRACT_NUM write SetLABELLING_CENTER_CONTRACT_NUM;
    //Дата договора с ЦЭМ
    property LABELLING_CENTER_CONTRACT_DATE:Tdate_type read FLABELLING_CENTER_CONTRACT_DATE write SetLABELLING_CENTER_CONTRACT_DATE;
    //Контактное лицо
    property CONTACT:Tstring255_type read FCONTACT write SetCONTACT;
    //Адрес доставки
    property DELIVERY_ADDRESS:Tstring255_type read FDELIVERY_ADDRESS write SetDELIVERY_ADDRESS;
    //Параметры товаров
    property PRODUCT_DETAILS:TMKIO_Order_PRODUCT_DETAILS read FPRODUCT_DETAILS;
    property action_id:Longint read Faction_id write Setaction_id;
  end;

  {  TMKIO_Order_element  }
  TMKIO_Order_element = class(TMKIO_Order)
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  {  TMKIO_Order_PRODUCT_DETAILS  }
  TMKIO_Order_PRODUCT_DETAILS = class(TXmlSerializationObject)
  private
    FPRODUCT:TMKIO_Order_PRODUCT_DETAILS_PRODUCTList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PRODUCT:TMKIO_Order_PRODUCT_DETAILS_PRODUCTList read FPRODUCT;
  end;

  {  TMKIO_Order_PRODUCT_DETAILS_PRODUCT  }
  TMKIO_Order_PRODUCT_DETAILS_PRODUCT = class(TXmlSerializationObject)
  private
    FGTIN:TGTIN;
    FTNVED_CODE:TTNVED_CODE;
    FRELEASE_METHOD:TRELEASE_METHOD;
    FMKIO_QUANTITY:Int64;
    FIO_TYPE:TIO_TYPE;
    FS_ORG_METHOD:TS_ORG_METHOD;
    FMARKING_TYPE:TMARKING_TYPE;
    FS_NUMS:TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS;
    procedure SetGTIN( AValue:TGTIN);
    procedure SetTNVED_CODE( AValue:TTNVED_CODE);
    procedure SetRELEASE_METHOD( AValue:TRELEASE_METHOD);
    procedure SetMKIO_QUANTITY( AValue:Int64);
    procedure SetIO_TYPE( AValue:TIO_TYPE);
    procedure SetS_ORG_METHOD( AValue:TS_ORG_METHOD);
    procedure SetMARKING_TYPE( AValue:TMARKING_TYPE);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //GTIN
    property GTIN:TGTIN read FGTIN write SetGTIN;
    //Код ТН ВЭД ЕАС
    property TNVED_CODE:TTNVED_CODE read FTNVED_CODE write SetTNVED_CODE;
    //Способ выпуска товаров в оборот
    //1 - Производство в РФ
    //2 - Импорт
    property RELEASE_METHOD:TRELEASE_METHOD read FRELEASE_METHOD write SetRELEASE_METHOD;
    //Количество КМ СИ. В случае указания в поле "Способ
    //формирования индивидуального серийного номера s" значения 1 - поле НЕ
    //обязательное для заполнения.
    property MKIO_QUANTITY:Int64 read FMKIO_QUANTITY write SetMKIO_QUANTITY;
    //Вид СИ
    //1 - Печатаемый
    //2 - Клеевой
    //3 - Навесной
    property IO_TYPE:TIO_TYPE read FIO_TYPE write SetIO_TYPE;
    //Способ формирования индивидуального серийного номера s
    //1 - Самостоятельно
    //2 - Оператором ИС МП
    property S_ORG_METHOD:TS_ORG_METHOD read FS_ORG_METHOD write SetS_ORG_METHOD;
    //Вид маркировки
    //1 - На потребительскую упаковку
    //2 - На товар
    //3 - На товарный ярлык
    property MARKING_TYPE:TMARKING_TYPE read FMARKING_TYPE write SetMARKING_TYPE;
    //Индивидуальные серийные номера. В случае указания в поле
    //"Способ формирования индивидуального серийного номера s" значения 1 -
    //поле обязательное для заполнения.
    property S_NUMS:TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS read FS_NUMS;
  end;

  {  TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS  }
  TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS = class(TXmlSerializationObject)
  private
    FSERIAL_NUMBER:TXSDStringArray;
    procedure SetSERIAL_NUMBER( AValue:TXSDStringArray);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Серийный номер
    property SERIAL_NUMBER:TXSDStringArray read FSERIAL_NUMBER write SetSERIAL_NUMBER;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  Faoguid:=AValue;
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  Fhouseguid:=AValue;
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
  Fflat:=AValue;
  ModifiedProperty('flat');
end;

procedure Tfias_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('aoguid', 'aoguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('houseguid', 'houseguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('flat', 'flat', [xsaSimpleObject], '', 1, 20);
end;

procedure Tfias_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfias_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfias_address_type.Create;
begin
  inherited Create;
end;

  {  TMKIO_Order  }
procedure TMKIO_Order.SetTRADE_PARTICIPANT_INN(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('TRADE_PARTICIPANT_INN', AValue);
  CheckStrMaxSize('TRADE_PARTICIPANT_INN', AValue);
  FTRADE_PARTICIPANT_INN:=AValue;
  ModifiedProperty('TRADE_PARTICIPANT_INN');
end;

procedure TMKIO_Order.SetOPERATOR_CONTRACT_NUM(AValue: Tstring255_type);
begin
  CheckStrMinSize('OPERATOR_CONTRACT_NUM', AValue);
  CheckStrMaxSize('OPERATOR_CONTRACT_NUM', AValue);
  FOPERATOR_CONTRACT_NUM:=AValue;
  ModifiedProperty('OPERATOR_CONTRACT_NUM');
end;

procedure TMKIO_Order.SetOPERATOR_CONTRACT_DATE(AValue: Tdate_type);
begin
  CheckStrMinSize('OPERATOR_CONTRACT_DATE', AValue);
  CheckStrMaxSize('OPERATOR_CONTRACT_DATE', AValue);
  FOPERATOR_CONTRACT_DATE:=AValue;
  ModifiedProperty('OPERATOR_CONTRACT_DATE');
end;

procedure TMKIO_Order.SetPREP_METHOD(AValue: TPREP_METHOD);
begin
  CheckLockupValue('PREP_METHOD', AValue);
  FPREP_METHOD:=AValue;
  ModifiedProperty('PREP_METHOD');
end;

procedure TMKIO_Order.SetRECEPTION_METHOD(AValue: TRECEPTION_METHOD);
begin
  CheckLockupValue('RECEPTION_METHOD', AValue);
  FRECEPTION_METHOD:=AValue;
  ModifiedProperty('RECEPTION_METHOD');
end;

procedure TMKIO_Order.SetLABEL_TEMPLATE_ID(AValue: Tguid_type);
begin
  CheckStrMinSize('LABEL_TEMPLATE_ID', AValue);
  CheckStrMaxSize('LABEL_TEMPLATE_ID', AValue);
  FLABEL_TEMPLATE_ID:=AValue;
  ModifiedProperty('LABEL_TEMPLATE_ID');
end;

procedure TMKIO_Order.SetLABELLING_CENTER_NAME(AValue: Tstring255_type);
begin
  CheckStrMinSize('LABELLING_CENTER_NAME', AValue);
  CheckStrMaxSize('LABELLING_CENTER_NAME', AValue);
  FLABELLING_CENTER_NAME:=AValue;
  ModifiedProperty('LABELLING_CENTER_NAME');
end;

procedure TMKIO_Order.SetLABELLING_CENTER_INN(AValue: TLABELLING_CENTER_INN_type);
begin
  CheckStrMinSize('LABELLING_CENTER_INN', AValue);
  CheckStrMaxSize('LABELLING_CENTER_INN', AValue);
  FLABELLING_CENTER_INN:=AValue;
  ModifiedProperty('LABELLING_CENTER_INN');
end;

procedure TMKIO_Order.SetLABELLING_CENTER_CONTRACT_NUM(AValue: Tstring255_type);
begin
  CheckStrMinSize('LABELLING_CENTER_CONTRACT_NUM', AValue);
  CheckStrMaxSize('LABELLING_CENTER_CONTRACT_NUM', AValue);
  FLABELLING_CENTER_CONTRACT_NUM:=AValue;
  ModifiedProperty('LABELLING_CENTER_CONTRACT_NUM');
end;

procedure TMKIO_Order.SetLABELLING_CENTER_CONTRACT_DATE(AValue: Tdate_type);
begin
  CheckStrMinSize('LABELLING_CENTER_CONTRACT_DATE', AValue);
  CheckStrMaxSize('LABELLING_CENTER_CONTRACT_DATE', AValue);
  FLABELLING_CENTER_CONTRACT_DATE:=AValue;
  ModifiedProperty('LABELLING_CENTER_CONTRACT_DATE');
end;

procedure TMKIO_Order.SetCONTACT(AValue: Tstring255_type);
begin
  CheckStrMinSize('CONTACT', AValue);
  CheckStrMaxSize('CONTACT', AValue);
  FCONTACT:=AValue;
  ModifiedProperty('CONTACT');
end;

procedure TMKIO_Order.SetDELIVERY_ADDRESS(AValue: Tstring255_type);
begin
  CheckStrMinSize('DELIVERY_ADDRESS', AValue);
  CheckStrMaxSize('DELIVERY_ADDRESS', AValue);
  FDELIVERY_ADDRESS:=AValue;
  ModifiedProperty('DELIVERY_ADDRESS');
end;

procedure TMKIO_Order.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure TMKIO_Order.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('TRADE_PARTICIPANT_INN', 'TRADE_PARTICIPANT_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('OPERATOR_CONTRACT_NUM', 'OPERATOR_CONTRACT_NUM', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('OPERATOR_CONTRACT_DATE', 'OPERATOR_CONTRACT_DATE', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('PREP_METHOD', 'PREP_METHOD', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('RECEPTION_METHOD', 'RECEPTION_METHOD', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('LABEL_TEMPLATE_ID', 'LABEL_TEMPLATE_ID', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('LABELLING_CENTER_NAME', 'LABELLING_CENTER_NAME', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('LABELLING_CENTER_INN', 'LABELLING_CENTER_INN', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('LABELLING_CENTER_CONTRACT_NUM', 'LABELLING_CENTER_CONTRACT_NUM', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('LABELLING_CENTER_CONTRACT_DATE', 'LABELLING_CENTER_CONTRACT_DATE', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('CONTACT', 'CONTACT', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('DELIVERY_ADDRESS', 'DELIVERY_ADDRESS', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('PRODUCT_DETAILS', 'PRODUCT_DETAILS', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='01';
end;

procedure TMKIO_Order.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT_DETAILS:=TMKIO_Order_PRODUCT_DETAILS.Create;
end;

destructor TMKIO_Order.Destroy;
begin
  FPRODUCT_DETAILS.Free;
  inherited Destroy;
end;

constructor TMKIO_Order.Create;
begin
  inherited Create;
  action_id:=01;
end;

  {  TMKIO_Order_element  }
procedure TMKIO_Order_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure TMKIO_Order_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TMKIO_Order_element.Destroy;
begin
  inherited Destroy;
end;

function TMKIO_Order_element.RootNodeName:string;
begin
  Result:='MKIO_Order';
end;

constructor TMKIO_Order_element.Create;
begin
  inherited Create;
end;

  {  TMKIO_Order_PRODUCT_DETAILS  }
procedure TMKIO_Order_PRODUCT_DETAILS.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('PRODUCT', 'PRODUCT', [], '', -1, -1);
end;

procedure TMKIO_Order_PRODUCT_DETAILS.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT:=TMKIO_Order_PRODUCT_DETAILS_PRODUCTList.Create;
end;

destructor TMKIO_Order_PRODUCT_DETAILS.Destroy;
begin
  FPRODUCT.Free;
  inherited Destroy;
end;

constructor TMKIO_Order_PRODUCT_DETAILS.Create;
begin
  inherited Create;
end;

  {  TMKIO_Order_PRODUCT_DETAILS_PRODUCT  }
procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetGTIN(AValue: TGTIN);
begin
  CheckStrMinSize('GTIN', AValue);
  CheckStrMaxSize('GTIN', AValue);
  FGTIN:=AValue;
  ModifiedProperty('GTIN');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetTNVED_CODE(AValue: TTNVED_CODE);
begin
  CheckMinInclusiveValue('TNVED_CODE', AValue);
  CheckMaxInclusiveValue('TNVED_CODE', AValue);
  FTNVED_CODE:=AValue;
  ModifiedProperty('TNVED_CODE');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetRELEASE_METHOD(AValue: TRELEASE_METHOD);
begin
  CheckLockupValue('RELEASE_METHOD', AValue);
  FRELEASE_METHOD:=AValue;
  ModifiedProperty('RELEASE_METHOD');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetMKIO_QUANTITY(AValue: Int64);
begin
  FMKIO_QUANTITY:=AValue;
  ModifiedProperty('MKIO_QUANTITY');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetIO_TYPE(AValue: TIO_TYPE);
begin
  CheckLockupValue('IO_TYPE', AValue);
  FIO_TYPE:=AValue;
  ModifiedProperty('IO_TYPE');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetS_ORG_METHOD(AValue: TS_ORG_METHOD);
begin
  CheckLockupValue('S_ORG_METHOD', AValue);
  FS_ORG_METHOD:=AValue;
  ModifiedProperty('S_ORG_METHOD');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.SetMARKING_TYPE(AValue: TMARKING_TYPE);
begin
  CheckLockupValue('MARKING_TYPE', AValue);
  FMARKING_TYPE:=AValue;
  ModifiedProperty('MARKING_TYPE');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('GTIN', 'GTIN', [xsaSimpleObject], '', 14, 14);
  P:=RegisterProperty('TNVED_CODE', 'TNVED_CODE', [xsaSimpleObject], '', -1, -1);
    P.minInclusiveInt:=6401;
    P.maxInclusiveInt:=6405;
  P:=RegisterProperty('RELEASE_METHOD', 'RELEASE_METHOD', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('MKIO_QUANTITY', 'MKIO_QUANTITY', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('IO_TYPE', 'IO_TYPE', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
  P:=RegisterProperty('S_ORG_METHOD', 'S_ORG_METHOD', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('MARKING_TYPE', 'MARKING_TYPE', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
  P:=RegisterProperty('S_NUMS', 'S_NUMS', [], '', -1, -1);
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FS_NUMS:=TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.Create;
end;

destructor TMKIO_Order_PRODUCT_DETAILS_PRODUCT.Destroy;
begin
  FS_NUMS.Free;
  inherited Destroy;
end;

constructor TMKIO_Order_PRODUCT_DETAILS_PRODUCT.Create;
begin
  inherited Create;
end;

  {  TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS  }
procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.SetSERIAL_NUMBER(AValue: TXSDStringArray);
var
  V:TSERIAL_NUMBER;
begin
  for V in AValue do
  begin
    CheckStrMinSize('SERIAL_NUMBER', V);
    CheckStrMaxSize('SERIAL_NUMBER', V);
  end;
  FSERIAL_NUMBER:=AValue;
  ModifiedProperty('SERIAL_NUMBER');
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('SERIAL_NUMBER', 'SERIAL_NUMBER', [xsaSimpleObject], '', 13, 13);
end;

procedure TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.Destroy;
begin
  inherited Destroy;
end;

constructor TMKIO_Order_PRODUCT_DETAILS_PRODUCT_S_NUMS.Create;
begin
  inherited Create;
end;

end.
