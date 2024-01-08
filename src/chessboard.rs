#[derive(Copy, Clone)]
#[repr(u8)]
pub enum PieceKind { K, Q, R, B, N, P }

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum PlayerSide { White, Black }

#[derive(Copy, Clone)]
pub struct Piece {
    pub side: PlayerSide,
    pub kind: PieceKind,
}
