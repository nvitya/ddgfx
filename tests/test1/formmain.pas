unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx;


type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExit : TButton;
    oglpanel : TPanel;
    pnl : TPanel;
    drawtimer : TTimer;
    procedure btnExitClick(Sender : TObject);
    procedure drawtimerTimer(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private

  public
    scene : TddScene;

    shp : TShape;      // the original of the shape
    grp : TDrawGroup;  // the original of the drawgroup

    grarr : array of TClonedGroup;

    t : single; // time

    origw, origh : integer;

    procedure OglboxResize(Sender: TObject);

  end;

var
  frmMain : TfrmMain;

implementation

uses dglOpenGL;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.drawtimerTimer(Sender : TObject);
var
  cgr : TClonedGroup;
  a : integer;
begin
  // modify the shapes
  t += 1;
  a := 0;
  for cgr in grarr do
  begin
    cgr.alpha := 1 - a * 0.2 + 0.5* sin(a * t / 50);

    cgr.scalex := sin((a + 1) * t / 100);
    Inc(a);
  end;

  // for proper rescaling effect the x,y should remain zero
  scene.root.x := 100 + 50*sin(t / 20);
  scene.root.y :=  50 + 50*cos(t / 20);

  scene.root.scalex := oglpanel.Width / origw;
  scene.root.scaley := oglpanel.Height / origh;

  scene.Repaint;
end;

procedure TfrmMain.FormCreate(Sender : TObject);
const
  triangle_verts : array[0..2] of TVertex = ((0, 0), (1, 0), (1, 1));
var
  csh : TClonedShape;
  cgr : TClonedGroup;
begin

  origw := oglpanel.Width;
  origh := oglpanel.Height;

  SetLength(grarr, 0);

  scene := TddScene.Create(self, oglpanel);

  scene.bgcolor.r := 0.4;
  scene.bgcolor.g := 0.4;
  scene.bgcolor.b := 0.2;

  shp := TShape.Create(nil);
  shp.AddPrimitive(GL_TRIANGLES, 3, @triangle_verts[0]);
  shp.x := 0;
  shp.y := 0;
  shp.scalex := 100;
  shp.scaley := 100;
  shp.alpha := 0.5;
  shp.rotation := 0;
  shp.SetColor(1, 0, 0, 1);

  grp := TDrawGroup.Create(nil);

  csh := grp.CloneShape(shp);

  csh := grp.CloneShape(shp);
  csh.x += 50;
  csh.rotation := 20;
  csh.SetColor(0, 1, 0, 0.4);

  csh := grp.CloneShape(shp);
  csh.x += 70;
  csh.rotation := 30;
  csh.SetColor(0, 0, 1, 0.4);

  cgr := scene.root.CloneGroup(grp);
  cgr.x := 100;
  cgr.y := 100;
  insert(cgr, grarr, length(grarr) + 1);

  cgr := scene.root.CloneGroup(grp);
  cgr.x := 200;
  cgr.y := 150;
  cgr.alpha := 0.5;
  insert(cgr, grarr, length(grarr) + 1);

  cgr := scene.root.CloneGroup(grp);
  cgr.x := 300;
  cgr.y := 170;
  cgr.alpha := 0.3;
  insert(cgr, grarr, length(grarr) + 1);

  //scene.bgcolor.r := 1;

  scene.OnResize := @OglboxResize;

  drawtimer.Enabled := true;
  t := 0;
end;

procedure TfrmMain.FormDestroy(Sender : TObject);
begin
  grp.Free;
  shp.Free;
end;

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;


end.

