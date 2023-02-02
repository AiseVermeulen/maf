package maf.modular.scheme.modactor

import maf.language.AScheme.ASchemeValues.Message
import maf.language.AScheme.ASchemeValues
import maf.util.datastructures.SmartUnion
import maf.util.Default
import maf.core.Lattice
import maf.language.AScheme.ASchemeValues.AbstractMessage
import maf.language.AScheme.ASchemeValues.MetaMessage

/**
 * An abstract representation of a mailbox.
 *
 * @tparam M
 *   the type of the messages in the mailbox
 * @tparam K
 *   the type of context of the messages in the mailbox
 */
trait AbstractMailbox[M, K]:
    /**
     * Enqueue a message in the mailbox.
     *
     * @param msg
     *   the message to enqueue in the mailbox
     * @return
     *   an updated abstract mailbox
     * @note
     *   implementations of this method may assume that the a sequence of <code>enqueue</code> commands entails a sending order of the messages in the
     *   mailbox.
     */
    def enqueue(msg: M, ctx: K): AbstractMailbox[M, K]

    /**
     * Pop a message from the mailbox. Depending on the level of abstraction used, it can return a set of messages that might be received during that
     * turn.
     */
    def dequeue: Set[((M, K), AbstractMailbox[M, K])] // TODO: rename to dequeue

    /** Returns all messages in the mailbox */
    def messages: Set[M]

    /** Merge the given mailbox with the current one */
    def merge(other: AbstractMailbox[M, K]): AbstractMailbox[M, K]

/**
 * A powerset implementation of the mailbox, as used in Emanuele D’Osualdo, Jonathan Kochems, and Luke Ong. Automatic verification of erlang- style
 * concurrency. In Static Analysis - 20th International Symposium, SAS 2013
 */
case class PowersetMailbox[M, K](msgs: Set[(M, K)]) extends AbstractMailbox[M, K]:
    def enqueue(msg: M, ctx: K): PowersetMailbox[M, K] =
        this.copy(msgs = msgs + ((msg, ctx)))

    def dequeue: Set[((M, K), AbstractMailbox[M, K])] =
        // We return a copy of the same mailbox for each message since message multiplicity is unknown
        msgs.map(m => (m, PowersetMailbox(msgs)))

    val messages: Set[M] = msgs.map(_._1)

    def merge(other: AbstractMailbox[M, K]): AbstractMailbox[M, K] = other match
        case PowersetMailbox(msgs) => this.copy(msgs = SmartUnion.sunion(this.msgs, msgs))

object PowersetMailbox:
    given [M, K]: Default[PowersetMailbox[M, K]] with
        def default: PowersetMailbox[M, K] = PowersetMailbox(Set())

/** This trait implements simple messages where the arguments of the messages are not store allocated */
trait SimpleMessageMailbox extends SchemeModActorSemantics:
    type Msg = AbstractMessage[Value]
    override def mkMessage(tpy: Value, arguments: Value): Msg = MetaMessage(tpy, arguments)

/** An analysis with a powerset mailbox */
trait PowersetMailboxAnalysis extends SchemeModActorSemantics:
    def emptyMailbox: Mailbox = PowersetMailbox(Set())
