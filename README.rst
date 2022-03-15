|build-status|

A minor mode for ``masukomi/private_comments``.  Private comments
appear as overlays and are not part of the source.

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

   Key             Binding
   -------------------------------------------------------------------------------
   C-c C-d		private-comments-delete
   C-c C-r		private-comments-record
   
   C-M-q		prog-indent-sexp

.. _Cask: https://github.com/cask/cask.git
