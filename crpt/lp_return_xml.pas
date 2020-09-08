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

unit lp_return_xml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects, LP_base_types_v2;

type

  {  Forward declarations  }
  Treturn = class;
  Treturn_element = class;
  Treturn_products_list = class;
  Treturn_products_list_product = class;

  {  Generic classes for collections  }
  TreturnList = specialize GXMLSerializationObjectList<Treturn>;
  Treturn_elementList = specialize GXMLSerializationObjectList<Treturn_element>;
  Treturn_products_listList = specialize GXMLSerializationObjectList<Treturn_products_list>;
  Treturn_products_list_productList = specialize GXMLSerializationObjectList<Treturn_products_list_product>;

  {  Treturn  }
  //Возврат в оборот
  Treturn = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Freturn_type:Treturn_type_type;
    Fpaid:Boolean;
    Fprimary_document_type:Twithdraw_primary_document_type_type;
    Fprimary_document_custom_name:Tstring255_type;
    Fprimary_document_number:Tstring255_type;
    Fprimary_document_date:Tdate_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    Fproducts_list:Treturn_products_list;
    Faction_id:Longint;
    Fversion:Double;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setreturn_type( AValue:Treturn_type_type);
    procedure Setpaid( AValue:Boolean);
    procedure Setprimary_document_type( AValue:Twithdraw_primary_document_type_type);
    procedure Setprimary_document_custom_name( AValue:Tstring255_type);
    procedure Setprimary_document_number( AValue:Tstring255_type);
    procedure Setprimary_document_date( AValue:Tdate_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Double);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Вид возврата
    property return_type:Treturn_type_type read Freturn_type write Setreturn_type;
    //Товар оплачен
    property paid:Boolean read Fpaid write Setpaid;
    //Тип первичного документа
    property primary_document_type:Twithdraw_primary_document_type_type read Fprimary_document_type write Setprimary_document_type;
    //Наименование первичного документа
    property primary_document_custom_name:Tstring255_type read Fprimary_document_custom_name write Setprimary_document_custom_name;
    //Номер первичного документа
    property primary_document_number:Tstring255_type read Fprimary_document_number write Setprimary_document_number;
    //Дата первичного документа
    property primary_document_date:Tdate_type read Fprimary_document_date write Setprimary_document_date;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
    //Параметры товаров
    property products_list:Treturn_products_list read Fproducts_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Double read Fversion write Setversion;
  end;

  {  Treturn_element  }
  Treturn_element = class(Treturn)
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

  {  Treturn_products_list  }
  Treturn_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Treturn_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Treturn_products_list_productList read Fproduct;
  end;

  {  Treturn_products_list_product  }
  Treturn_products_list_product = class(TXmlSerializationObject)
  private
    Fki:Tki_type;
    Fpaid:Boolean;
    Fprimary_document_type:Twithdraw_primary_document_type_type;
    Fprimary_document_custom_name:Tstring255_type;
    Fprimary_document_number:Tstring255_type;
    Fprimary_document_date:Tdate_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    procedure Setki( AValue:Tki_type);
    procedure Setpaid( AValue:Boolean);
    procedure Setprimary_document_type( AValue:Twithdraw_primary_document_type_type);
    procedure Setprimary_document_custom_name( AValue:Tstring255_type);
    procedure Setprimary_document_number( AValue:Tstring255_type);
    procedure Setprimary_document_date( AValue:Tdate_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Код идентификации
    property ki:Tki_type read Fki write Setki;
    //Товар оплачен
    property paid:Boolean read Fpaid write Setpaid;
    //Тип первичного документа
    property primary_document_type:Twithdraw_primary_document_type_type read Fprimary_document_type write Setprimary_document_type;
    //Наименование первичного документа
    property primary_document_custom_name:Tstring255_type read Fprimary_document_custom_name write Setprimary_document_custom_name;
    //Номер первичного документа
    property primary_document_number:Tstring255_type read Fprimary_document_number write Setprimary_document_number;
    //Дата первичного документа
    property primary_document_date:Tdate_type read Fprimary_document_date write Setprimary_document_date;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
  end;

implementation

  {  Treturn  }
procedure Treturn.Settrade_participant_inn(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Treturn.Setreturn_type(AValue: Treturn_type_type);
begin
  CheckLockupValue('return_type', AValue);
  Freturn_type:=AValue;
  ModifiedProperty('return_type');
end;

procedure Treturn.Setpaid(AValue: Boolean);
begin
  Fpaid:=AValue;
  ModifiedProperty('paid');
end;

procedure Treturn.Setprimary_document_type(AValue: Twithdraw_primary_document_type_type);
begin
  CheckLockupValue('primary_document_type', AValue);
  Fprimary_document_type:=AValue;
  ModifiedProperty('primary_document_type');
end;

procedure Treturn.Setprimary_document_custom_name(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_custom_name', AValue);
  CheckStrMaxSize('primary_document_custom_name', AValue);
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure Treturn.Setprimary_document_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_number', AValue);
  CheckStrMaxSize('primary_document_number', AValue);
  Fprimary_document_number:=AValue;
  ModifiedProperty('primary_document_number');
end;

procedure Treturn.Setprimary_document_date(AValue: Tdate_type);
begin
  CheckStrMinSize('primary_document_date', AValue);
  CheckStrMaxSize('primary_document_date', AValue);
  Fprimary_document_date:=AValue;
  ModifiedProperty('primary_document_date');
end;

procedure Treturn.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  CheckLockupValue('certificate_type', AValue);
  Fcertificate_type:=AValue;
  ModifiedProperty('certificate_type');
end;

procedure Treturn.Setcertificate_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  Fcertificate_number:=AValue;
  ModifiedProperty('certificate_number');
end;

procedure Treturn.Setcertificate_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  Fcertificate_date:=AValue;
  ModifiedProperty('certificate_date');
end;

procedure Treturn.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Treturn.Setversion(AValue: Double);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Treturn.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('return_type', 'return_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('RETAIL_RETURN');
    P.ValidList.Add('REMOTE_SALE_RETURN');
  P:=RegisterProperty('paid', 'paid', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('primary_document_type', 'primary_document_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('RECEIPT');
    P.ValidList.Add('STRICT_REPORTING_FORM');
    P.ValidList.Add('CONTRACT');
    P.ValidList.Add('DESTRUCTION_ACT');
    P.ValidList.Add('CONSIGNMENT_NOTE');
    P.ValidList.Add('UTD');
    P.ValidList.Add('OTHER');
  P:=RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_number', 'primary_document_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_date', 'primary_document_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='34';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1';
end;

procedure Treturn.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Treturn_products_list.Create;
end;

destructor Treturn.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Treturn.Create;
begin
  inherited Create;
  action_id:=34;
  version:=1;
end;

  {  Treturn_element  }
procedure Treturn_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Treturn_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treturn_element.Destroy;
begin
  inherited Destroy;
end;

function Treturn_element.RootNodeName:string;
begin
  Result:='return';
end;

constructor Treturn_element.Create;
begin
  inherited Create;
end;

  {  Treturn_products_list  }
procedure Treturn_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Treturn_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Treturn_products_list_productList.Create;
end;

destructor Treturn_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Treturn_products_list.Create;
begin
  inherited Create;
end;

  {  Treturn_products_list_product  }
procedure Treturn_products_list_product.Setki(AValue: Tki_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Treturn_products_list_product.Setpaid(AValue: Boolean);
begin
  Fpaid:=AValue;
  ModifiedProperty('paid');
end;

procedure Treturn_products_list_product.Setprimary_document_type(AValue: Twithdraw_primary_document_type_type);
begin
  CheckLockupValue('primary_document_type', AValue);
  Fprimary_document_type:=AValue;
  ModifiedProperty('primary_document_type');
end;

procedure Treturn_products_list_product.Setprimary_document_custom_name(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_custom_name', AValue);
  CheckStrMaxSize('primary_document_custom_name', AValue);
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure Treturn_products_list_product.Setprimary_document_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('primary_document_number', AValue);
  CheckStrMaxSize('primary_document_number', AValue);
  Fprimary_document_number:=AValue;
  ModifiedProperty('primary_document_number');
end;

procedure Treturn_products_list_product.Setprimary_document_date(AValue: Tdate_type);
begin
  CheckStrMinSize('primary_document_date', AValue);
  CheckStrMaxSize('primary_document_date', AValue);
  Fprimary_document_date:=AValue;
  ModifiedProperty('primary_document_date');
end;

procedure Treturn_products_list_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  CheckLockupValue('certificate_type', AValue);
  Fcertificate_type:=AValue;
  ModifiedProperty('certificate_type');
end;

procedure Treturn_products_list_product.Setcertificate_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  Fcertificate_number:=AValue;
  ModifiedProperty('certificate_number');
end;

procedure Treturn_products_list_product.Setcertificate_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  Fcertificate_date:=AValue;
  ModifiedProperty('certificate_date');
end;

procedure Treturn_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('paid', 'paid', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('primary_document_type', 'primary_document_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('RECEIPT');
    P.ValidList.Add('STRICT_REPORTING_FORM');
    P.ValidList.Add('CONTRACT');
    P.ValidList.Add('DESTRUCTION_ACT');
    P.ValidList.Add('CONSIGNMENT_NOTE');
    P.ValidList.Add('UTD');
    P.ValidList.Add('OTHER');
  P:=RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_number', 'primary_document_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_date', 'primary_document_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
end;

procedure Treturn_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treturn_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Treturn_products_list_product.Create;
begin
  inherited Create;
end;

end.