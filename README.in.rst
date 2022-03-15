|build-status|

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://github.com/dickmao/private-comments-mode/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/private-comments-mode/actions
   :alt: Build Status

.. |--| unicode:: U+2013   .. en dash
.. |---| unicode:: U+2014  .. em dash, trimming surrounding whitespace
   :trim:

Install
=======
::

   git clone https://github.com/commercial-emacs/private-comments-mode.git
   make -C private-comments-mode install

Usage
=====
::

   M-x private-comments-mode

Keymap (C-h b)
==============

::

.. KEYS NOTEBOOK (see Makefile)

Caveats
=======
Equal (``=``) characters are substituted with hyphens (``-``).
The ``private_comments`` server returns ``Internal Server 500`` when sent
equal characters, even from ``pc``.

.. _Cask: https://github.com/cask/cask.git
