{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  base on docs from http://api-docs.diadoc.ru

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

unit Vvod_Imp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  TDocType = String;
  TDeclaration_Number = String;
  TCustoms_Code = String;
  TDecision_Code = Longint;
  TCertificate_Doc = Longint;
  TCertificate_Doc_Number = String;
  TTNVED_Code = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  TVvod_Imp = class;
  TVvod_Imp_element = class;
  TVvod_Imp_Products_List = class;
  TVvod_Imp_Products_List_PRODUCT = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  TVvod_ImpList = specialize GXMLSerializationObjectList<TVvod_Imp>;
  TVvod_Imp_elementList = specialize GXMLSerializationObjectList<TVvod_Imp_element>;
  TVvod_Imp_Products_ListList = specialize GXMLSerializationObjectList<TVvod_Imp_Products_List>;
  TVvod_Imp_Products_List_PRODUCTList = specialize GXMLSerializationObjectList<TVvod_Imp_Products_List_PRODUCT>;

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

  {  TVvod_Imp  }
  //Ввод товаров в оборот (импорт)
  TVvod_Imp = class(TXmlSerializationObject)
  private
    FDocType:TDocType;
    FTRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type;
    FDeclaration_Date:Tdate_type;
    FDeclaration_Number:TDeclaration_Number;
    FCustoms_Code:TCustoms_Code;
    FDecision_Code:TDecision_Code;
    FProducts_List:TVvod_Imp_Products_List;
    Faction_id:String;
    Fversion:Longint;
    procedure SetDocType( AValue:TDocType);
    procedure SetTRADE_PARTICIPANT_INN( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure SetDeclaration_Date( AValue:Tdate_type);
    procedure SetDeclaration_Number( AValue:TDeclaration_Number);
    procedure SetCustoms_Code( AValue:TCustoms_Code);
    procedure SetDecision_Code( AValue:TDecision_Code);
    procedure Setaction_id( AValue:String);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DocType:TDocType read FDocType write SetDocType;
    //ИНН участника оборота
    property TRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type read FTRADE_PARTICIPANT_INN write SetTRADE_PARTICIPANT_INN;
    //Дата декларации на товары
    property Declaration_Date:Tdate_type read FDeclaration_Date write SetDeclaration_Date;
    //Регистрационный номер ДТ
    property Declaration_Number:TDeclaration_Number read FDeclaration_Number write SetDeclaration_Number;
    //Код таможенного органа
    property Customs_Code:TCustoms_Code read FCustoms_Code write SetCustoms_Code;
    //Код принятого решения
    //10 - Выпуск товаров разрешен
    //11 - Выпуск товаров при условии обеспечения исполнения обязанности по уплате таможенных пошлин, налогов, специальных, антидемпинговых, компенсационных пошлин, за исключением выпуска товаров, поименованного в позициях с кодами 12 и 13
    //12 - Выпуск товаров с особенностями, предусмотренными статьей 121 Таможенного кодекса Евразийского экономического союза
    //13 - Выпуск товаров с особенностями, предусмотренными статьей 122 Таможенного кодекса Евразийского экономического союза
    //14 - Выпуск товаров с особенностями, предусмотренными статьей 123 Таможенного кодекса Евразийского экономического союза
    //20 - Условный выпуск товаров
    property Decision_Code:TDecision_Code read FDecision_Code write SetDecision_Code;
    property Products_List:TVvod_Imp_Products_List read FProducts_List;
    property action_id:String read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  TVvod_Imp_element  }
  TVvod_Imp_element = class(TVvod_Imp)
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

  {  TVvod_Imp_Products_List  }
  TVvod_Imp_Products_List = class(TXmlSerializationObject)
  private
    FPRODUCT:TVvod_Imp_Products_List_PRODUCTList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PRODUCT:TVvod_Imp_Products_List_PRODUCTList read FPRODUCT;
  end;

  {  TVvod_Imp_Products_List_PRODUCT  }
  TVvod_Imp_Products_List_PRODUCT = class(TXmlSerializationObject)
  private
    FUIT:Tgs1_uit_type;
    FUITU:Tgs1_uitu_type;
    FCertificate_Doc:TCertificate_Doc;
    FCertificate_Doc_Number:TCertificate_Doc_Number;
    FCertificate_Doc_Date:Tdate_type;
    FTNVED_Code:TTNVED_Code;
    procedure SetUIT( AValue:Tgs1_uit_type);
    procedure SetUITU( AValue:Tgs1_uitu_type);
    procedure SetCertificate_Doc( AValue:TCertificate_Doc);
    procedure SetCertificate_Doc_Number( AValue:TCertificate_Doc_Number);
    procedure SetCertificate_Doc_Date( AValue:Tdate_type);
    procedure SetTNVED_Code( AValue:TTNVED_Code);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //УИТ
    property UIT:Tgs1_uit_type read FUIT write SetUIT;
    //УИТУ
    property UITU:Tgs1_uitu_type read FUITU write SetUITU;
    //Вид документа Обязательной Сертификации
    //1 - Сертификат соответствия
    //2 - Декларация соответствия
    property Certificate_Doc:TCertificate_Doc read FCertificate_Doc write SetCertificate_Doc;
    //Номер документа обязательной сертификации
    property Certificate_Doc_Number:TCertificate_Doc_Number read FCertificate_Doc_Number write SetCertificate_Doc_Number;
    //Дата документа обязательной сертификации
    property Certificate_Doc_Date:Tdate_type read FCertificate_Doc_Date write SetCertificate_Doc_Date;
    property TNVED_Code:TTNVED_Code read FTNVED_Code write SetTNVED_Code;
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

  {  TVvod_Imp  }
procedure TVvod_Imp.SetDocType(AValue: TDocType);
begin
  CheckLockupValue('DocType', AValue);
  FDocType:=AValue;
  ModifiedProperty('DocType');
end;

procedure TVvod_Imp.SetTRADE_PARTICIPANT_INN(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('TRADE_PARTICIPANT_INN', AValue);
  CheckStrMaxSize('TRADE_PARTICIPANT_INN', AValue);
  FTRADE_PARTICIPANT_INN:=AValue;
  ModifiedProperty('TRADE_PARTICIPANT_INN');
end;

procedure TVvod_Imp.SetDeclaration_Date(AValue: Tdate_type);
begin
  CheckStrMinSize('Declaration_Date', AValue);
  CheckStrMaxSize('Declaration_Date', AValue);
  FDeclaration_Date:=AValue;
  ModifiedProperty('Declaration_Date');
end;

procedure TVvod_Imp.SetDeclaration_Number(AValue: TDeclaration_Number);
begin
  CheckStrMinSize('Declaration_Number', AValue);
  CheckStrMaxSize('Declaration_Number', AValue);
  FDeclaration_Number:=AValue;
  ModifiedProperty('Declaration_Number');
end;

procedure TVvod_Imp.SetCustoms_Code(AValue: TCustoms_Code);
begin
  CheckStrMinSize('Customs_Code', AValue);
  CheckStrMaxSize('Customs_Code', AValue);
  FCustoms_Code:=AValue;
  ModifiedProperty('Customs_Code');
end;

procedure TVvod_Imp.SetDecision_Code(AValue: TDecision_Code);
begin
  CheckLockupValue('Decision_Code', AValue);
  FDecision_Code:=AValue;
  ModifiedProperty('Decision_Code');
end;

procedure TVvod_Imp.Setaction_id(AValue: String);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure TVvod_Imp.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure TVvod_Imp.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('DocType', 'DocType', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('GOODSIMPORT');
  P:=RegisterProperty('TRADE_PARTICIPANT_INN', 'TRADE_PARTICIPANT_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('Declaration_Date', 'Declaration_Date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('Declaration_Number', 'Declaration_Number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('Customs_Code', 'Customs_Code', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('Decision_Code', 'Decision_Code', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('10');
    P.ValidList.Add('11');
    P.ValidList.Add('12');
    P.ValidList.Add('13');
    P.ValidList.Add('14');
    P.ValidList.Add('20');
  P:=RegisterProperty('Products_List', 'Products_List', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='5.1';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure TVvod_Imp.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProducts_List:=TVvod_Imp_Products_List.Create;
end;

destructor TVvod_Imp.Destroy;
begin
  FProducts_List.Free;
  inherited Destroy;
end;

constructor TVvod_Imp.Create;
begin
  inherited Create;
  action_id:='5.1';
  version:=2;
end;

  {  TVvod_Imp_element  }
procedure TVvod_Imp_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure TVvod_Imp_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TVvod_Imp_element.Destroy;
begin
  inherited Destroy;
end;

function TVvod_Imp_element.RootNodeName:string;
begin
  Result:='Vvod_Imp';
end;

constructor TVvod_Imp_element.Create;
begin
  inherited Create;
end;

  {  TVvod_Imp_Products_List  }
procedure TVvod_Imp_Products_List.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('PRODUCT', 'PRODUCT', [], '', -1, -1);
end;

procedure TVvod_Imp_Products_List.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT:=TVvod_Imp_Products_List_PRODUCTList.Create;
end;

destructor TVvod_Imp_Products_List.Destroy;
begin
  FPRODUCT.Free;
  inherited Destroy;
end;

constructor TVvod_Imp_Products_List.Create;
begin
  inherited Create;
end;

  {  TVvod_Imp_Products_List_PRODUCT  }
procedure TVvod_Imp_Products_List_PRODUCT.SetUIT(AValue: Tgs1_uit_type);
begin
  CheckStrMinSize('UIT', AValue);
  CheckStrMaxSize('UIT', AValue);
  FUIT:=AValue;
  ModifiedProperty('UIT');
end;

procedure TVvod_Imp_Products_List_PRODUCT.SetUITU(AValue: Tgs1_uitu_type);
begin
  CheckStrMinSize('UITU', AValue);
  CheckStrMaxSize('UITU', AValue);
  FUITU:=AValue;
  ModifiedProperty('UITU');
end;

procedure TVvod_Imp_Products_List_PRODUCT.SetCertificate_Doc(AValue: TCertificate_Doc);
begin
  CheckLockupValue('Certificate_Doc', AValue);
  FCertificate_Doc:=AValue;
  ModifiedProperty('Certificate_Doc');
end;

procedure TVvod_Imp_Products_List_PRODUCT.SetCertificate_Doc_Number(AValue: TCertificate_Doc_Number);
begin
  CheckStrMinSize('Certificate_Doc_Number', AValue);
  CheckStrMaxSize('Certificate_Doc_Number', AValue);
  FCertificate_Doc_Number:=AValue;
  ModifiedProperty('Certificate_Doc_Number');
end;

procedure TVvod_Imp_Products_List_PRODUCT.SetCertificate_Doc_Date(AValue: Tdate_type);
begin
  CheckStrMinSize('Certificate_Doc_Date', AValue);
  CheckStrMaxSize('Certificate_Doc_Date', AValue);
  FCertificate_Doc_Date:=AValue;
  ModifiedProperty('Certificate_Doc_Date');
end;

procedure TVvod_Imp_Products_List_PRODUCT.SetTNVED_Code(AValue: TTNVED_Code);
begin
  CheckStrMinSize('TNVED_Code', AValue);
  CheckStrMaxSize('TNVED_Code', AValue);
  FTNVED_Code:=AValue;
  ModifiedProperty('TNVED_Code');
end;

procedure TVvod_Imp_Products_List_PRODUCT.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('UIT', 'UIT', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('UITU', 'UITU', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('Certificate_Doc', 'Certificate_Doc', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('Certificate_Doc_Number', 'Certificate_Doc_Number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('Certificate_Doc_Date', 'Certificate_Doc_Date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('TNVED_Code', 'TNVED_Code', [xsaSimpleObject], '', 10, 10);
end;

procedure TVvod_Imp_Products_List_PRODUCT.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TVvod_Imp_Products_List_PRODUCT.Destroy;
begin
  inherited Destroy;
end;

constructor TVvod_Imp_Products_List_PRODUCT.Create;
begin
  inherited Create;
end;

end.
