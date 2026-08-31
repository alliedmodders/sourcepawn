class Node {
    int value;
    Node next;
    Node parent;
}

class Tree {
    Tree left;
    Tree right;
    int data[4];
}

public OnPluginStart()
{
    Node a = new Node();
    Node b = new Node();
    a.next = b;
    b.parent = a;
    a.value = 10;

    Tree t = new Tree();
    Tree l = new Tree();
    Tree r = new Tree();
    t.left = l;
    t.right = r;
    l.data[0] = 1;

    Node c = null;
    if (c == null)
        c = a;
}
