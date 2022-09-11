unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx, datawave;


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

    grp : TDrawGroup;

    tria : TShape;

    waves : array of TDataWave;

    t : single; // time

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
  n : integer;
  w : TDataWave;
begin
  t := t + 1;
  w := waves[0];
  w.shp.x := -1000 + 1000*sin(t / 20);
  for n := 1 to 2 do
  begin
    waves[n].shp.x := w.shp.x;
  end;
  scene.Repaint;
end;

procedure TfrmMain.FormCreate(Sender : TObject);
const
  triangle_verts : array[0..2] of TVertex = ((0, 0), (1, 0), (1, 1));
var
  n : integer;
  w : TDataWave;
begin

  scene := TddScene.Create(self, oglpanel);

  grp := TDrawGroup.Create(scene.root);

  SetLength(waves, 0);
  for n := 0 to 2 do
  begin
    w := TDataWave.Create(grp);
    w.GenerateRandomData(100000);
    w.shp.y := 80 * (n + 1);
    w.shp.x := 10;
    w.shp.alpha := 0.6;
    w.shp.scalex := 100000 / length(w.data);
    insert(w, waves, length(waves));
  end;

  waves[0].shp.SetColor(1.0, 0.0, 0.0);
  waves[1].shp.SetColor(0.0, 1.0, 0.0);
  waves[2].shp.SetColor(0.5, 0.5, 1.0);

  tria := TShape.Create(scene.root);
  tria.AddPrimitive(GL_TRIANGLES, 3, @triangle_verts[0]);
  tria.x := 200;
  tria.y := 200;
  tria.scalex := 100;
  tria.scaley := 100;
  tria.alpha := 0.1;
  tria.rotation := 0;
  tria.SetColor(1, 0, 0, 1);

  scene.bgcolor.r := 0.2;
  scene.bgcolor.g := 0.2;
  scene.bgcolor.b := 0.2;

  scene.OnResize := @OglboxResize;

  drawtimer.Enabled := true;
  t := 0;
end;

procedure TfrmMain.FormDestroy(Sender : TObject);
var
  w : TDataWave;
begin
  grp.Free;

  for w in waves do w.Free;
  SetLength(waves, 0);

end;

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;

end.

