(in-package :html5-parser)

(defun make-document ()
  (cxml-dom:create-document))

(defun make-fragment (document)
  (dom:create-document-fragment document))

(defun make-doctype (document name public-id system-id)
  (let ((doctype (dom:create-document-type (dom:implementation document) name public-id system-id)))
    (setf (slot-value doctype 'rune-dom::owner) document)
    doctype))

(defun make-comment (document data)
  (dom:create-comment document data))

(defun make-element (document name namespace)
  (if namespace
      (dom:create-element-ns document namespace name)
      (dom:create-element document name)))

(defun make-text-node (document data)
  (dom:create-text-node document data))


(defun node-type (node)
  (dom:node-type node))

(defun node-name (node)
  (dom:node-name node))

(defun node-value (node)
  (dom:node-value node))

(defun (setf node-value) (new-value node)
  (setf (dom:node-value node) new-value))

(defun node-namespace (node)
  (dom:namespace-uri node))

(defun node-public-id (node)
  (dom:public-id node))

(defun node-system-id (node)
  (dom:system-id node))


(defun element-attribute (node attribute &optional namespace)
  (if namespace
      (dom:get-attribute-ns node namespace attribute)
      (dom:get-attribute node attribute)))

(defun (setf element-attribute) (new-value node attribute
                                 &optional namespace)
  (if namespace
      (dom:set-attribute-ns node namespace attribute new-value)
      (dom:set-attribute node attribute new-value)))

(defun element-map-attributes (function node)
  (dom:map-node-map (lambda (attr)
                      (funcall function
                               (dom:node-name attr)
                               (dom:namespace-uri attr)
                               (dom:node-value attr)))
                    (dom:attributes node)))

(defun element-map-children (function node)
  (let (kids)
    (dom:map-node-list (lambda (kid)
                         (push kid kids))
                       (dom:child-nodes node))
    (map nil function (nreverse kids))))

(defun node-append-child (node new-child)
  (dom:append-child node new-child))

(defun node-remove-child (node child)
  (dom:remove-child node child))

(defun node-insert-before (node new-child ref-child)
  (dom:insert-before node new-child ref-child))

(defun node-first-child (node)
  (dom:first-child node))

(defun node-last-child (node)
  (dom:last-child node))

(defun node-previous-sibling (node)
  (dom:previous-sibling node))

(defun node-next-sibling (node)
  (dom:next-sibling node))

(defun node-parent (node)
  (dom:parent-node node))
