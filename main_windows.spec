# -*- mode: python -*-

block_cipher = None

a = Analysis(['main.py'],
             pathex=[
			 'C:\\mercados_webcrawler',
			 'C:\\Python36-32\\lib\\site-packages'],
             binaries=None,
             datas=[
('pylibs','pylibs'),
('logo','logo'),
('kivylibs','kivylibs'),
('rlibs','rlibs'),
('rscripts','rscripts'),
('pyscripts","pyscripts')],
             hiddenimports=['six','packaging','packaging.version','packaging.specifiers','packaging.requirements','appdirs','kivy','opencv-python','win32timezone'],
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
          name='Mercados_Webscraper',
          debug=False,
          strip=False,
          upx=True,
	      icon='logo\\logo.ico',
          console=True )
