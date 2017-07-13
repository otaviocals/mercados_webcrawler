# -*- mode: python -*-

block_cipher = None

a = Analysis(['main.py'],
             pathex=["/home/cals/Projects/mercados_webcrawler"],
             binaries=None,
             datas=[
("pylibs","pylibs"),
("logo","logo"),
("kivylibs","kivylibs"),
("rlibs","rlibs"),
("rscripts","rscripts"),
("pyscripts","pyscripts")],
             hiddenimports=["six","packaging","packaging.version","packaging.specifiers","packaging.requirements","appdirs","kivy", "opencv-python","numpy"],
             hookspath=["."],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='SUSEP_Webscraper',
          debug=False,
          strip=False,
          upx=True,
          icon='logo.ico',
          console=False )
