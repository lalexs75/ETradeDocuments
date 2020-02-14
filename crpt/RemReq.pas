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

unit RemReq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  TPARTICIPANT_INN = String;
  TREMARKING_CAUSE = Longint;
  TREMARKING_CAUSE1 = Longint;
  TPROD_CERT_DOC_TYPE = Longint;
  TPROD_CERT_DOC_NUM = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  TRemReq = class;
  TRemReq_element = class;
  TRemReq_PRODUCT_DETAILS = class;
  TRemReq_PRODUCT_DETAILS_PRODUCT = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  TRemReqList = specialize GXMLSerializationObjectList<TRemReq>;
  TRemReq_elementList = specialize GXMLSerializationObjectList<TRemReq_element>;
  TRemReq_PRODUCT_DETAILSList = specialize GXMLSerializationObjectList<TRemReq_PRODUCT_DETAILS>;
  TRemReq_PRODUCT_DETAILS_PRODUCTList = specialize GXMLSerializationObjectList<TRemReq_PRODUCT_DETAILS_PRODUCT>;

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

  {  TRemReq  }
  //Заявка на повторную маркировку
  TRemReq = class(TXmlSerializationObject)
  private
    FPARTICIPANT_INN:TPARTICIPANT_INN;
    FREMARKING_DATE:Tdate_type;
    FREMARKING_CAUSE:TREMARKING_CAUSE;
    FPRODUCT_DETAILS:TRemReq_PRODUCT_DETAILS;
    Faction_id:Longint;
    procedure SetPARTICIPANT_INN( AValue:TPARTICIPANT_INN);
    procedure SetREMARKING_DATE( AValue:Tdate_type);
    procedure SetREMARKING_CAUSE( AValue:TREMARKING_CAUSE);
    procedure Setaction_id( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН
    property PARTICIPANT_INN:TPARTICIPANT_INN read FPARTICIPANT_INN write SetPARTICIPANT_INN;
    //Дата повторной маркировки
    property REMARKING_DATE:Tdate_type read FREMARKING_DATE write SetREMARKING_DATE;
    //Причина повторной маркировки
    //1 - Испорчено либо утеряно СИ с КМ
    //2 - Выявлены ошибки описания товара
    property REMARKING_CAUSE:TREMARKING_CAUSE read FREMARKING_CAUSE write SetREMARKING_CAUSE;
    //Параметры товаров
    property PRODUCT_DETAILS:TRemReq_PRODUCT_DETAILS read FPRODUCT_DETAILS;
    property action_id:Longint read Faction_id write Setaction_id;
  end;

  {  TRemReq_element  }
  TRemReq_element = class(TRemReq)
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

  {  TRemReq_PRODUCT_DETAILS  }
  TRemReq_PRODUCT_DETAILS = class(TXmlSerializationObject)
  private
    FPRODUCT:TRemReq_PRODUCT_DETAILS_PRODUCTList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PRODUCT:TRemReq_PRODUCT_DETAILS_PRODUCTList read FPRODUCT;
  end;

  {  TRemReq_PRODUCT_DETAILS_PRODUCT  }
  TRemReq_PRODUCT_DETAILS_PRODUCT = class(TXmlSerializationObject)
  private
    FREMARKING_DATE:Tdate_type;
    FREMARKING_CAUSE:TREMARKING_CAUSE1;
    FLAST_UIT:Tgs1_uit_type;
    FNEW_UIT:Tgs1_uit_type;
    FPROD_CERT_DOC_TYPE:TPROD_CERT_DOC_TYPE;
    FPROD_CERT_DOC_NUM:TPROD_CERT_DOC_NUM;
    FPROD_CERT_DOC_DATE:Tdate_type;
    procedure SetREMARKING_DATE( AValue:Tdate_type);
    procedure SetREMARKING_CAUSE( AValue:TREMARKING_CAUSE1);
    procedure SetLAST_UIT( AValue:Tgs1_uit_type);
    procedure SetNEW_UIT( AValue:Tgs1_uit_type);
    procedure SetPROD_CERT_DOC_TYPE( AValue:TPROD_CERT_DOC_TYPE);
    procedure SetPROD_CERT_DOC_NUM( AValue:TPROD_CERT_DOC_NUM);
    procedure SetPROD_CERT_DOC_DATE( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Дата повторной маркировки
    property REMARKING_DATE:Tdate_type read FREMARKING_DATE write SetREMARKING_DATE;
    //Причина повторной маркировки
    //1 - Испорчено либо утеряно СИ с КМ
    //2 - Выявлены ошибки описания товара
    property REMARKING_CAUSE:TREMARKING_CAUSE1 read FREMARKING_CAUSE write SetREMARKING_CAUSE;
    //Предыдущий УИТ
    property LAST_UIT:Tgs1_uit_type read FLAST_UIT write SetLAST_UIT;
    //Новый УИТ
    property NEW_UIT:Tgs1_uit_type read FNEW_UIT write SetNEW_UIT;
    //Вид документа обязательной сертификации
    //1 - Сертификат соответствия
    //2 - Декларация соответствия
    property PROD_CERT_DOC_TYPE:TPROD_CERT_DOC_TYPE read FPROD_CERT_DOC_TYPE write SetPROD_CERT_DOC_TYPE;
    //Номер документа обязательной сертификации
    property PROD_CERT_DOC_NUM:TPROD_CERT_DOC_NUM read FPROD_CERT_DOC_NUM write SetPROD_CERT_DOC_NUM;
    //Дата документа обязательной сертификации
    property PROD_CERT_DOC_DATE:Tdate_type read FPROD_CERT_DOC_DATE write SetPROD_CERT_DOC_DATE;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  Faoguid:=AValue;
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  Fhouseguid:=AValue;
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  Fflat:=AValue;
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
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

  {  TRemReq  }
procedure TRemReq.SetPARTICIPANT_INN(AValue: TPARTICIPANT_INN);
begin
  FPARTICIPANT_INN:=AValue;
  CheckStrMinSize('PARTICIPANT_INN', AValue);
  CheckStrMaxSize('PARTICIPANT_INN', AValue);
  ModifiedProperty('PARTICIPANT_INN');
end;

procedure TRemReq.SetREMARKING_DATE(AValue: Tdate_type);
begin
  FREMARKING_DATE:=AValue;
  CheckStrMinSize('REMARKING_DATE', AValue);
  CheckStrMaxSize('REMARKING_DATE', AValue);
  ModifiedProperty('REMARKING_DATE');
end;

procedure TRemReq.SetREMARKING_CAUSE(AValue: TREMARKING_CAUSE);
begin
  FREMARKING_CAUSE:=AValue;
  CheckLockupValue('REMARKING_CAUSE', AValue);
  ModifiedProperty('REMARKING_CAUSE');
end;

procedure TRemReq.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure TRemReq.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('PARTICIPANT_INN', 'PARTICIPANT_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('REMARKING_DATE', 'REMARKING_DATE', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('REMARKING_CAUSE', 'REMARKING_CAUSE', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('PRODUCT_DETAILS', 'PRODUCT_DETAILS', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='10';
end;

procedure TRemReq.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT_DETAILS:=TRemReq_PRODUCT_DETAILS.Create;
end;

destructor TRemReq.Destroy;
begin
  FPRODUCT_DETAILS.Free;
  inherited Destroy;
end;

constructor TRemReq.Create;
begin
  inherited Create;
  action_id:=10;
end;

  {  TRemReq_element  }
procedure TRemReq_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure TRemReq_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TRemReq_element.Destroy;
begin
  inherited Destroy;
end;

function TRemReq_element.RootNodeName:string;
begin
  Result:='RemReq';
end;

constructor TRemReq_element.Create;
begin
  inherited Create;
end;

  {  TRemReq_PRODUCT_DETAILS  }
procedure TRemReq_PRODUCT_DETAILS.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('PRODUCT', 'PRODUCT', [], '', -1, -1);
end;

procedure TRemReq_PRODUCT_DETAILS.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT:=TRemReq_PRODUCT_DETAILS_PRODUCTList.Create;
end;

destructor TRemReq_PRODUCT_DETAILS.Destroy;
begin
  FPRODUCT.Free;
  inherited Destroy;
end;

constructor TRemReq_PRODUCT_DETAILS.Create;
begin
  inherited Create;
end;

  {  TRemReq_PRODUCT_DETAILS_PRODUCT  }
procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetREMARKING_DATE(AValue: Tdate_type);
begin
  FREMARKING_DATE:=AValue;
  CheckStrMinSize('REMARKING_DATE', AValue);
  CheckStrMaxSize('REMARKING_DATE', AValue);
  ModifiedProperty('REMARKING_DATE');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetREMARKING_CAUSE(AValue: TREMARKING_CAUSE1);
begin
  FREMARKING_CAUSE:=AValue;
  CheckLockupValue('REMARKING_CAUSE', AValue);
  ModifiedProperty('REMARKING_CAUSE');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetLAST_UIT(AValue: Tgs1_uit_type);
begin
  FLAST_UIT:=AValue;
  CheckStrMinSize('LAST_UIT', AValue);
  CheckStrMaxSize('LAST_UIT', AValue);
  ModifiedProperty('LAST_UIT');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetNEW_UIT(AValue: Tgs1_uit_type);
begin
  FNEW_UIT:=AValue;
  CheckStrMinSize('NEW_UIT', AValue);
  CheckStrMaxSize('NEW_UIT', AValue);
  ModifiedProperty('NEW_UIT');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetPROD_CERT_DOC_TYPE(AValue: TPROD_CERT_DOC_TYPE);
begin
  FPROD_CERT_DOC_TYPE:=AValue;
  CheckLockupValue('PROD_CERT_DOC_TYPE', AValue);
  ModifiedProperty('PROD_CERT_DOC_TYPE');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetPROD_CERT_DOC_NUM(AValue: TPROD_CERT_DOC_NUM);
begin
  FPROD_CERT_DOC_NUM:=AValue;
  CheckStrMaxSize('PROD_CERT_DOC_NUM', AValue);
  ModifiedProperty('PROD_CERT_DOC_NUM');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.SetPROD_CERT_DOC_DATE(AValue: Tdate_type);
begin
  FPROD_CERT_DOC_DATE:=AValue;
  CheckStrMinSize('PROD_CERT_DOC_DATE', AValue);
  CheckStrMaxSize('PROD_CERT_DOC_DATE', AValue);
  ModifiedProperty('PROD_CERT_DOC_DATE');
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('REMARKING_DATE', 'REMARKING_DATE', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('REMARKING_CAUSE', 'REMARKING_CAUSE', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('LAST_UIT', 'LAST_UIT', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('NEW_UIT', 'NEW_UIT', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('PROD_CERT_DOC_TYPE', 'PROD_CERT_DOC_TYPE', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('PROD_CERT_DOC_NUM', 'PROD_CERT_DOC_NUM', [xsaSimpleObject], '', -1, 1000);
  P:=RegisterProperty('PROD_CERT_DOC_DATE', 'PROD_CERT_DOC_DATE', [xsaSimpleObject], '', 10, 10);
end;

procedure TRemReq_PRODUCT_DETAILS_PRODUCT.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TRemReq_PRODUCT_DETAILS_PRODUCT.Destroy;
begin
  inherited Destroy;
end;

constructor TRemReq_PRODUCT_DETAILS_PRODUCT.Create;
begin
  inherited Create;
end;

end.